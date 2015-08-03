package utils

import scala.concurrent.Future

import scalaz._
import Scalaz._
import scalaz.Validation.FlatMap._

import play.api._
import play.api.Logger
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.libs.json._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import play.api.libs.functional.syntax._

object Database {

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

  case class Relationship (
    id: Int,
    start: String,
    property: String,
    self: String,
    properties: String,
    rtype: String,
    end: String
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

  implicit val relationshipReads: Reads[Relationship] = (
    (JsPath \ "metadata" \ "id").read[Int] and
    (JsPath \ "start").read[String] and
    (JsPath \ "property").read[String] and
    (JsPath \ "self").read[String] and
    (JsPath \ "properties").read[String] and
    (JsPath \ "type").read[String] and
    (JsPath \ "end").read[String]
  )(Relationship.apply _)

  type Error = String

  val dblog = Logger(this.getClass())

  val address = Play.current.configuration.getString("neo4j.address").get

  val user = Play.current.configuration.getString("neo4j.user").get

  val password = Play.current.configuration.getString("neo4j.password").get

  val queryPath = "http://" + address + "/db/data/"

  def withAuth (path: String): WSRequest =
    WS.url(path).withAuth(user, password, WSAuthScheme.BASIC)

  def statusMustBe (response: WSResponse, status: Int, info: String): Validation[Error, WSResponse] = {
    if (response.status != status) {
      val error = "Faulty response when attempting to (" + info + "): " + response.toString
      dblog.error(error)
      Failure(error)
    } else {
      Success(response)
    }
  }

  def deserializeNode (obj: JsValue): Validation[Error , Node] = obj.validate[Node] match {
    case s: JsSuccess[Node] => Success(s.get)
    case e: JsError =>
      println(obj.toString)
      val error = "JsError when trying to deserialize a Node: " + JsError.toJson(e).toString()
      dblog.error(error)
      Failure(error)
  }

  def deserializeRealtionship (obj: JsValue): Validation[Error , Relationship] = obj.validate[Relationship] match {
    case s: JsSuccess[Relationship] => Success(s.get)
    case e: JsError =>
      val error = "JsError when trying to deserialize a Relationship: " + JsError.toJson(e).toString()
      dblog.error(error)
      Failure(error)
  }

  def validateAnd[A] (response: WSResponse, status: Int, attempt: String)(deserialize: JsValue => Validation[Error, A]): Validation[Error, A] = for {
    _ <- statusMustBe(response, status, attempt)
    node <- deserialize(response.json)
  } yield node

  def reachable: Future[Validation[Error, WSResponse]] = for {
    response <- withAuth(queryPath).head()
    validation <- Future(statusMustBe(response, 200, "reach the database"))
  } yield validation

  def getNode (id: Int): Future[Validation[Error, Node]] = for {
    response <- withAuth(queryPath + "node/" + id).get()
    node <- Future(validateAnd(response, 200, "get node")(deserializeNode))
  } yield node

  def createNode (properties: JsValue): Future[Validation[Error, Node]] = for {
    response <- withAuth(queryPath + "node").post(properties)
    node <- Future(validateAnd(response, 201, "create node")(deserializeNode))
  } yield node

  def addLabel (node: Node, label: String): Future[Validation[Error, WSResponse]] = for {
      response <- withAuth(node.labels).post(JsString(label))
      validation <- Future(statusMustBe(response, 204, "add label"))
  } yield validation

  def deleteNode (node: Node): Future[Validation[Error, WSResponse]] = for {
      response <- withAuth(node.self).delete()
      validation <- Future(statusMustBe(response, 204, "delete node"))
  } yield validation

  def getRelationship (id: Int): Future[Validation[Error, Relationship]] = for {
    response <- withAuth(queryPath + "relationship/" + id).get()
    relationship <- Future(validateAndDeserializeRelationship(response, 200, "get node"))
  } yield relationship

}
