package utils

import scala.concurrent._

import scalaz.{Validation, Success, Failure}
import play.api.Logger
import play.api.libs.json._
import play.api.libs.ws.WSResponse
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.ValidationOps._

trait Persistent[E <: Entity, P] {
  def instantiate (props: P, node: Node): E
}

object PersistentOps {

  private def instantiation[E <: Entity, P] (props: P, node: Node)(implicit per: Persistent[E, P]): Future[Validation[Err, E]] =
    Future(Success(per.instantiate(props, node)))

  private def instantiationA[E <: Entity, P] (props: P, node: Validation[Err, Node])(implicit per: Persistent[E, P]): Future[Validation[Err, E]] =
    ifSucceeds(node)(instantiation(props, _))

  private def instantiationB[E <: Entity, P] (props: JsValue, node: Validation[Err, Node])(implicit propsReads: Reads[P], per: Persistent[E, P]): Future[Validation[Err, E]] =
    props.validate[P] match {
      case p: JsSuccess[P] =>
        instantiationA(p.get, node)
      case e: JsError =>
        val error = DeserializationErr("JsError when trying to deserialize an Entity: " + JsError.toJson(e).toString())
        Logger.error(error.toString)
        Future(Failure(error))
    }

  private def instantiationC[E <: Entity, P] (props: Validation[Err, JsValue], node: Validation[Err, Node])(implicit propsReads: Reads[P], per: Persistent[E, P]): Future[Validation[Err, E]] =
    ifSucceeds(props, node) { (props, node) => props.validate[P] match {
      case p: JsSuccess[P] =>
        instantiation(p.get, node)
      case e: JsError =>
        val error = DeserializationErr("JsError when trying to deserialize an Entity: " + JsError.toJson(e).toString())
        Logger.error(error.toString)
        Future(Failure(error))
    }}

  def create[E <: Entity, P] (props: P)(implicit propsWrites: Writes[P], t: Tagged[E], per: Persistent[E, P]): Future[Validation[Err, E]] =
    for {
      tx <- openTransaction
      result <- executeCreation(tx, props)
      node <- extractNode(result)
      entity <- instantiationA(props, node)
      _ <- allOrNothing(tx, entity)
    } yield entity

  private def executeCreation[E <: Entity, P] (tx: Validation[Err, Transaction], props: P)(implicit propsWrites: Writes[P], t: Tagged[E], per: Persistent[E, P]): Future[Validation[Err, TxResult]] =
    execute(tx, Json.arr(Json.obj(
      "statement" -> ("CREATE (n:" + t.tag + " {props}) RETURN n"),
      "parameters" -> Json.obj("props" -> Json.toJson(props)),
      "resultDataContents" -> Json.arr("REST")
    )))

  private def extractNode (txr: Validation[Err, TxResult]): Future[Validation[Err, Node]] =
    ifSucceeds(txr) { result: TxResult =>
      (result.head("n") \ "rest")(0).validate[Node] match {
        case s: JsSuccess[Node] =>
          Future(Success(s.get))
        case e: JsError =>
          val error = DeserializationErr("JsError when trying to deserialize a Node: " + JsError.toJson(e).toString())
          Logger.error(error.toString)
          Future(Failure(error))
      }
    }

  def get[E <: Entity, P] (id: Int)(implicit propsReads: Reads[P], per: Persistent[E, P]): Future[Validation[Err, E]] = for {
    node <- getNode(id)
    props <- getNodeProperties(node)
    entity <- instantiationC[E, P](props, node)
  } yield entity

  def delete[E <: Entity] (entity: Validation[Err, E])(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    ifSucceeds(entity) { entity: E =>
      for {
        tx <- openTransaction
        deleteResult <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n:" + t.tag + ") WHERE id(n)=" + entity.node.id.toString + " DELETE n"))))
        _ <- allOrNothing(tx, deleteResult)
      } yield deleteResult
    }

  def deleteRelationships[E <: Entity] (entity: Validation[Err, E])(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    ifSucceeds(entity) { entity: E =>
      for {
        tx <- openTransaction
        deleteResult <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n:" + t.tag + ")-[r]-() WHERE id(n)=" + entity.node.id.toString + " DELETE r"))))
        _ <- allOrNothing(tx, deleteResult)
      } yield deleteResult
    }

  def set[E <: Entity] (entity: Validation[Err, E], prop: String, value: JsValue): Future[Validation[Err, WSResponse]] =
    ifSucceeds(entity) { entity: E => setNodeProperty(Success(entity.node), prop, value) }

  def count[E <: Entity] (implicit t: Tagged[E]): Future[Validation[Err, Int]] = for {
    tx <- openTransaction
    txr <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n: " + t.tag + ") RETURN count(n)"))))
    count <- extractCount(txr)
  } yield count

  private def extractCount (txr: Validation[Err, TxResult]): Future[Validation[Err, Int]] =
    ifSucceeds(txr) { result: TxResult =>
      Future(Success((result.head("count(n)") \ "row")(0).as[Int]))
    }
}
