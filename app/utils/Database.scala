package utils

import scala.concurrent.Future

import scalaz.{Validation, Success, Failure}
import scalaz.Validation.FlatMap._

import play.api._
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.libs.json._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import play.api.libs.functional.syntax._

import utils.ValidationOps._

object DatabaseOps {

  case class Node (
    id: Int,
    outgoingRelationships: String,
    labels: String,
    allTypedRelationships: String,
    traverse: String,
    self: String,
    property: String,
    properties: String,
    outgoingTypedRelationships: String,
    incomingRelationships: String,
    createRelationship: String,
    pagedTraverse: String,
    allRelationships: String,
    incomingTypedRelationships: String
  )

  implicit val nodeReads: Reads[Node] = (
    (JsPath \ "metadata" \ "id").read[Int] and
    (JsPath \ "outgoing_relationships").read[String] and
    (JsPath \ "labels").read[String] and
    (JsPath \ "all_typed_relationships").read[String] and
    (JsPath \ "traverse").read[String] and
    (JsPath \ "self").read[String] and
    (JsPath \ "property").read[String] and
    (JsPath \ "properties").read[String] and
    (JsPath \ "outgoing_typed_relationships").read[String] and
    (JsPath \ "incoming_relationships").read[String] and
    (JsPath \ "create_relationship").read[String] and
    (JsPath \ "paged_traverse").read[String] and
    (JsPath \ "all_relationships").read[String] and
    (JsPath \ "incoming_typed_relationships").read[String]
  )(Node.apply _)

  case class Relationship (
    id: Int,
    start: String,
    property: String,
    self: String,
    properties: String,
    rtype: String,
    end: String
  )

  implicit val relationshipReads: Reads[Relationship] = (
    (JsPath \ "metadata" \ "id").read[Int] and
    (JsPath \ "start").read[String] and
    (JsPath \ "property").read[String] and
    (JsPath \ "self").read[String] and
    (JsPath \ "properties").read[String] and
    (JsPath \ "type").read[String] and
    (JsPath \ "end").read[String]
  )(Relationship.apply _)

  case class Transaction (self: String, commit: String, expires: String)

  private implicit val transactionReads: Reads[Transaction] = (
    (JsPath \ "commit").read[String] and
    (JsPath \ "commit").read[String] and
    (JsPath \ "transaction" \ "expires").read[String]
  )(Transaction.apply _)

  case class DeserializedTxErr (code: String, message: String)

  private implicit val transactionErrReads: Reads[DeserializedTxErr] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "message").read[String]
  )(DeserializedTxErr.apply _)

  case class TxErr (message: String) extends Err

  case class DeserializationErr (message: String) extends Err

  case class StatusErr (message: String) extends Err

  type TxResult = List[Map[String, JsValue]]

  private val dblog = Logger(this.getClass())

  private val address = Play.current.configuration.getString("neo4j.address").get

  private val user = Play.current.configuration.getString("neo4j.user").get

  private val password = Play.current.configuration.getString("neo4j.password").get

  private val queryPath = "http://" + address + "/db/data/"

  private val transactionalEndpoint = "http://" + address + "/db/data/transaction/"

  /**
    * Util Functions
    */

  private def withAuth (path: String): WSRequest =
    WS.url(path).withAuth(user, password, WSAuthScheme.BASIC)

  private def statusMustBe (response: WSResponse, status: Int, info: String): Validation[Err, WSResponse] = {
    if (response.status != status) {
      val error = StatusErr("Faulty response when attempting to (" + info + "): " + response.toString)
      dblog.error(error.toString)
      Failure(error)
    } else {
      Success(response)
    }
  }

  private def deserializeNode (response: WSResponse): Validation[Err, Node] = response.json.validate[Node] match {
    case s: JsSuccess[Node] => Success(s.get)
    case e: JsError =>
      val error = DeserializationErr("JsError when trying to deserialize a Node: " + JsError.toJson(e).toString())
      dblog.error(error.toString)
      Failure(error)
  }

  private def deserializeRelationship (response: WSResponse): Validation[Err, Relationship] = response.json.validate[Relationship] match {
    case s: JsSuccess[Relationship] => Success(s.get)
    case e: JsError =>
      val error = DeserializationErr("JsError when trying to deserialize a Relationship: " + JsError.toJson(e).toString())
      dblog.error(error.toString)
      Failure(error)
  }

  private def getJson (response: WSResponse): Validation[Err, JsValue] = Success(response.json)

  /**
    * Util Database
    */

  def reachable: Future[Validation[Err, WSResponse]] = for {
    response <- withAuth(queryPath).head()
    validation <- Future(statusMustBe(response, 200, "reach the database"))
  } yield validation

  /**
    * Node
    */

  def getNode (id: Int): Future[Validation[Err, Node]] = for {
    response <- withAuth(queryPath + "node/" + id).get()
    validation <- Future(for {
      _ <- statusMustBe(response, 200, "get node")
      node <- deserializeNode(response)
    } yield node)
  } yield validation

  def createNode (properties: JsValue): Future[Validation[Err, Node]] = for {
    response <- withAuth(queryPath + "node").post(properties)
    validation <- Future(for {
      _ <- statusMustBe(response, 201, "create node")
      node <- deserializeNode(response)
    } yield node)
  } yield validation

  def createNode: Future[Validation[Err, Node]] = createNode(Json.obj())

  def addLabel (node: Validation[Err, Node], label: String): Future[Validation[Err, WSResponse]] =
    ifSucceeds(node) { node: Node =>
      for {
        response <- withAuth(node.labels).post(JsString(label))
        validation <- Future(statusMustBe(response, 204, "add label"))
      } yield validation
    }

  def setNodeProperty (node: Validation[Err, Node], prop: String, value: JsValue): Future[Validation[Err, WSResponse]] =
    ifSucceeds(node) { node: Node =>
      for {
        response <- withAuth(node.properties + "/" + prop).put(value)
        validation <- Future(statusMustBe(response, 204, "set a node property"))
      } yield validation
    }

  def updateNodeProperties (node: Validation[Err, Node], properties: JsValue): Future[Validation[Err, WSResponse]] =
    ifSucceeds(node) { node: Node =>
      for {
        response <- withAuth(node.properties).put(properties)
        validation <- Future(statusMustBe(response, 204, "update node properties"))
      } yield validation
    }

  def getNodeProperties (node: Validation[Err, Node]): Future[Validation[Err, JsValue]] =
    ifSucceeds(node) { node: Node =>
      for {
        response <- withAuth(node.properties).get()
        validation <- Future(for {
          _ <- statusMustBe(response, 200, "get node properties")
          json <- getJson(response)
        } yield json)
      } yield validation
    }

  def deleteNode (node: Validation[Err, Node]): Future[Validation[Err, WSResponse]] =
    ifSucceeds(node) { node: Node =>
      for {
        response <- withAuth(node.self).delete()
        validation <- Future(statusMustBe(response, 204, "delete node"))
      } yield validation
    }

  /**
    * Relationshipa
    */

  def getRelationship (id: Int): Future[Validation[Err, Relationship]] = for {
    response <- withAuth(queryPath + "relationship/" + id).get()
    validation <- Future(for {
      _ <- statusMustBe(response, 200, "get node")
      relationship <- deserializeRelationship(response)
    } yield relationship)
  } yield validation

  def createRelationship (source: Validation[Err, Node], target: Validation[Err, Node], relType: String): Future[Validation[Err, Relationship]] =
    ifSucceeds(source, target) { (source: Node, target: Node) =>
      for {
        response <- withAuth(source.createRelationship).post(Json.obj(
          "to" -> target.self,
          "type" -> relType
        ))
        validation <- Future(for {
          _ <- statusMustBe(response, 201, "create relationship")
          relationship <- deserializeRelationship(response)
        } yield relationship)
      } yield validation
    }

  def createRelationship (source: Validation[Err, Node], target: Validation[Err, Node], relType: String, data: JsValue): Future[Validation[Err, Relationship]] =
    ifSucceeds(source, target) { (source: Node, target: Node) =>
      for {
        response <- withAuth(source.createRelationship).post(Json.obj(
          "to" -> target.self,
          "type" -> relType,
          "data" -> data
        ))
        validation <- Future(for {
          _ <- statusMustBe(response, 201, "create relationship")
          relationship <- deserializeRelationship(response)
        } yield relationship)
      } yield validation
    }

  def deleteRelationship (relationship: Validation[Err, Relationship]): Future[Validation[Err, WSResponse]] =
    ifSucceeds(relationship) { relationship: Relationship =>
      for {
        response <- withAuth(relationship.self).delete()
        validation <- Future(statusMustBe(response, 204, "delete relationship"))
      } yield validation
    }

  /**
    * Transactions
    */

  private def checkForTransactionErrs (response: WSResponse): Validation[Err, WSResponse] = (response.json \ "errors").validate[Seq[DeserializedTxErr]] match {
    case s: JsSuccess[List[Transaction]@unchecked] =>
      if (s.get.length > 0)
        Failure(TxErr(s.get.toString))
      else
        Success(response)
    case e: JsError =>
      val error = DeserializationErr("JsError when trying to deserialize errors in a transaction: " + JsError.toJson(e).toString())
      dblog.error(error.toString)
      Failure(error)
  }

  private def getTransaction (response: WSResponse): Validation[Err, Transaction] = response.json.validate[Transaction] match {
    case s: JsSuccess[Transaction] =>
      val old = s.get
      Success(Transaction(old.commit.reverse.drop(7).reverse, old.commit, old.expires))
    case e: JsError =>
      val error = DeserializationErr("JsError when trying to deserialize a transaction: " + JsError.toJson(e).toString())
      dblog.error(error.toString)
      Failure(error)
  }

  private def getTxResult (response: WSResponse): Validation[Err, TxResult] =
    Success((response.json \ "results").as[List[JsValue]].map(extractResult(_)))

  private def extractResult (js: JsValue): Map[String, JsValue] =
    ((js \ "columns").as[List[String]] zip (js \ "data").as[List[JsValue]]).toMap

  def openTransaction: Future[Validation[Err, Transaction]] = for {
    response <- withAuth(transactionalEndpoint).post(Json.obj("statements" -> Json.arr()))
    validation <- Future(for {
      _ <- statusMustBe(response, 201, "open a transaction")
      _ <- checkForTransactionErrs(response)
      transaction <- getTransaction(response)
    } yield transaction)
  } yield validation

  def execute (tx: Validation[Err, Transaction], statements: JsValue): Future[Validation[Err, TxResult]] =
    ifSucceeds(tx) { tx: Transaction =>
      for {
        response <- withAuth(tx.self).post(Json.obj("statements" -> statements))
        validation <- Future(for {
          _ <- statusMustBe(response, 200, "execute statements in transaction")
          _ <- checkForTransactionErrs(response)
          result <- getTxResult(response)
        } yield result)
      } yield validation
    }

  def commit (tx: Validation[Err, Transaction]): Future[Validation[Err, WSResponse]] =
    ifSucceeds(tx) { tx: Transaction =>
      for {
        response <- withAuth(tx.commit).post(Json.obj("statements" -> Json.arr()))
        validation <- Future(for {
          _ <- statusMustBe(response, 200, "commit a transaction")
          wsresponse <- checkForTransactionErrs(response)
        } yield wsresponse)
      } yield validation
    }

  def rollback (tx: Validation[Err, Transaction]): Future[Validation[Err, WSResponse]] =
    ifSucceeds(tx) { tx: Transaction =>
      for {
        response <- withAuth(tx.self).delete()
        validation <- Future(for {
          _ <- statusMustBe(response, 200, "rollback a transaction")
          wsresponse <- checkForTransactionErrs(response)
        } yield wsresponse)
      } yield validation
    }

  def allOrNothing[A] (tx: Validation[Err, Transaction], fin: Validation[Err, A]): Future[Validation[Err, WSResponse]] = fin match {
    case Success(_) => commit(tx)
    case Failure(_) => rollback(tx)
  }
}
