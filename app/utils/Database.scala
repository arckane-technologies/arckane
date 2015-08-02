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

  implicit val nodeReads: Reads[Node] = (
    (JsPath \ "metadata" \ "id").read[Int] and
    (JsPath \ "outgoingRelationships").read[String] and
    (JsPath \ "labels").read[String] and
    (JsPath \ "allTypedRelationships").read[String] and
    (JsPath \ "traverse").read[String] and
    (JsPath \ "self").read[String] and
    (JsPath \ "property").read[String] and
    (JsPath \ "properties").read[String] and
    (JsPath \ "outgoingTypedRelationships").read[String] and
    (JsPath \ "incomingRelationships").read[String] and
    (JsPath \ "createRelationship").read[String] and
    (JsPath \ "pagedTraverse").read[String] and
    (JsPath \ "allRelationships").read[String] and
    (JsPath \ "incomingTypedRelationships").read[String]
  )(Node.apply _)

  type Error = String

  val dblog = Logger(this.getClass())

  val address = Play.current.configuration.getString("neo4j.address").get

  val user = Play.current.configuration.getString("neo4j.user").get

  val password = Play.current.configuration.getString("neo4j.password").get

  val queryPath = "http://" + address + "/db/data/"

  def withAuth (path: String): WSRequest =
    WS.url(queryPath + path).withAuth(user, password, WSAuthScheme.BASIC)

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
      val error = JsError.toJson(e).toString()
      dblog.error(error)
      Failure(error)
  }

  def validateAndDeserialize (response: WSResponse, status: Int, attempt: String): Validation[Error, Node] = for {
    _ <- statusMustBe(response, 200, "get node")
    node <- deserializeNode(response.json)
  } yield node

  def reachable: Future[Validation[Error, WSResponse]] = for {
    response <- withAuth("").head()
    validation <- Future(statusMustBe(response, 200, "reach the database"))
  } yield validation

  def getNode (node: Int): Future[Validation[Error, Node]] = for {
    response <- withAuth("node/" + node.toString).get()
    node <- Future(validateAndDeserialize(response, 200, "get node"))
  } yield node

  def createNode (properties: JsValue): Future[Validation[Error, Node]] = for {
    response <- withAuth("node").post(properties)
    node <- Future(validateAndDeserialize(response, 201, "create node"))
  } yield node

  def addLabel (node: Int, label: String): Future[Validation[Error, WSResponse]] = for {
      response <- withAuth("node/" + node.toString + "/labels").post(JsString(label))
      validation <- Future(statusMustBe(response, 204, "add label"))
  } yield validation

}
