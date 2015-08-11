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
  def instantiate (props: P, url: String, node: Node): E
}

object PersistentOps {

  def getNode[E <: Entity, P] (tx: Validation[Err, Transaction], url: String)(implicit t: Tagged[E]): Future[Validation[Err, Node]] =
    for {
      result <- executeGet(tx, url)
      node <- extractRESTNode(result)
    } yield node

  def create[E <: Entity, P] (props: P)(implicit propsWrites: Writes[P], t: Tagged[E], per: Persistent[E, P]): Future[Validation[Err, E]] =
    for {
      tx <- openTransaction
      url <- newUrl(tx)
      result <- executeCreation(tx, url, props)
      node <- extractRESTNode(result)
      entity <- ifSucceedsF(url, node)(per.instantiate(props, _, _))
      _ <- allOrNothing(tx, entity)
    } yield entity

  def get[E <: Entity, P] (url: String)(implicit propsReads: Reads[P], t: Tagged[E], per: Persistent[E, P]): Future[Validation[Err, E]] =
    for {
      tx <- openTransaction
      node <- getNode(tx, url)
      props <- getNodeProperties(node)
      entity <- ifSucceeds(props, node) { (props, node) =>
                  ifSucceeds(props.validate[P])(per.instantiate(_, url, node))
                }
      _ <- allOrNothing(tx, entity)
    } yield entity

  def set[E <: Entity] (entity: E, prop: String, value: JsValue): Future[Validation[Err, WSResponse]] =
    setNodeProperty(Success(entity.node), prop, value)

  def set[E <: Entity] (entity: Validation[Err, E], prop: String, value: JsValue): Future[Validation[Err, WSResponse]] =
    ifSucceeds(entity)(set(_, prop, value))

  def delete[E <: Entity] (entity: Validation[Err, E])(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    ifSucceeds(entity) { entity: E =>
      for {
        tx <- openTransaction
        result <- executeDelete(tx, entity)
        _ <- allOrNothing(tx, result)
      } yield result
    }

  def deleteRelationships[E <: Entity] (entity: Validation[Err, E])(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    ifSucceeds(entity) { entity: E =>
      for {
        tx <- openTransaction
        deleteResult <- executeDeleteRels(tx, entity)
        _ <- allOrNothing(tx, deleteResult)
      } yield deleteResult
    }

  def count[E <: Entity] (implicit t: Tagged[E]): Future[Validation[Err, Int]] = for {
    tx <- openTransaction
    txr <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n: " + t.tag + ") RETURN count(n)"))))
    count <- ifSucceeds(txr) { result: TxResult =>
      ifSucceeds((result.head("count(n)") \ "row")(0).validate[Int])(identity)
    }
  } yield count

  private def newUrl[E <: Entity] (tx: Validation[Err, Transaction])(implicit t: Tagged[E]): Future[Validation[Err, String]] =
    for {
      result <- execute(tx, "MERGE (id:UniqueId{name:'General Entities IDs'}) ON CREATE SET id.count = 1 ON MATCH SET id.count = id.count + 1 RETURN id.count")
      url <- ifSucceeds(result) { result: TxResult =>
        ifSucceeds((result.head("id.count") \ "row")(0).validate[Int]) { uid: Int =>
          ("/"+t.tag.toLowerCase+"/"+uid.toString)
        }
      }
    } yield url

  private def executeCreation[E <: Entity, P] (tx: Validation[Err, Transaction], url: Validation[Err, String], props: P)(implicit propsWrites: Writes[P], t: Tagged[E]): Future[Validation[Err, TxResult]] =
    ifSucceeds(url) { url: String =>
      execute(tx, Json.arr(Json.obj(
        "statement" -> ("CREATE (n:"+t.tag+" {props}) RETURN n"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj("url" -> url) ++ Json.toJson(props).as[JsObject])
        ),
        "resultDataContents" -> Json.arr("REST")
      )))
    }

  private def executeGet[E <: Entity, P] (tx: Validation[Err, Transaction], url: String)(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    execute(tx, Json.arr(Json.obj(
      "statement" -> ("MATCH (n:"+t.tag+" {url: {urlmatcher}}) RETURN n"),
      "parameters" -> Json.obj(
        "urlmatcher" -> url
      ),
      "resultDataContents" -> Json.arr("REST")
    )))

  private def executeDelete[E <: Entity] (tx: Validation[Err, Transaction], entity: E)(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    execute(tx, Json.arr(Json.obj(
      "statement" -> ("MATCH (n:"+t.tag+" {url: {urlmatcher}}) DELETE n"),
      "parameters" -> Json.obj(
        "urlmatcher" -> entity.url
      )
    )))

  private def executeDeleteRels[E <: Entity] (tx: Validation[Err, Transaction], entity: E)(implicit t: Tagged[E]): Future[Validation[Err, TxResult]] =
    execute(tx, Json.arr(Json.obj(
      "statement" -> ("MATCH (n:"+t.tag+" {url: {urlmatcher}})-[r]-() DELETE r"),
      "parameters" -> Json.obj(
        "urlmatcher" -> entity.url
      )
    )))

  private def extractRESTNode (txr: Validation[Err, TxResult]): Future[Validation[Err, Node]] =
    ifSucceeds(txr) { result: TxResult => ifSucceeds((result.head("n") \ "rest")(0).validate[Node])(identity) }
}
