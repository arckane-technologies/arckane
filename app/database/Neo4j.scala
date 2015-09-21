package database

import scala.concurrent.Future

import play.api._
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.libs.json._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._

package object neo4j {

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

  implicit val nodeWrites = new Writes[Node] {
    def writes(node: Node) = Json.obj(
      "metadata" -> Json.obj("id" -> node.id),
      "outgoing_relationships" -> node.outgoingRelationships,
      "labels" -> node.labels,
      "all_typed_relationships" -> node.allTypedRelationships,
      "traverse" -> node.traverse,
      "self" -> node.self,
      "property" -> node.property,
      "properties" -> node.properties,
      "outgoing_typed_relationships" -> node.outgoingTypedRelationships,
      "incoming_relationships" -> node.incomingRelationships,
      "create_relationship" -> node.createRelationship,
      "paged_traverse" -> node.pagedTraverse,
      "all_relationships" -> node.allRelationships,
      "incoming_typed_relationships" -> node.incomingTypedRelationships
    )
  }

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

  implicit val relationshipWrites = new Writes[Relationship] {
    def writes(relationship: Relationship) = Json.obj(
      "metadata" -> Json.obj("id" -> relationship.id),
      "start" -> relationship.start,
      "property" -> relationship.property,
      "self" -> relationship.self,
      "properties" -> relationship.properties,
      "type" -> relationship.rtype,
      "end" -> relationship.end
    )
  }

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

  case class DeserializedTxErr (code: String, message: Option[String])

  private implicit val transactionErrReads: Reads[DeserializedTxErr] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "message").readNullable[String]
  )(DeserializedTxErr.apply _)

  case class TxErr (message: String) extends RuntimeException(message)

  case class DeserializationErr (message: String) extends RuntimeException(message)

  case class StatusErr (message: String) extends RuntimeException(message)

  /* First list is for results of every cypher query, then a map for each match and their list of value. */
  type TxResult = List[Map[String, List[JsValue]]]

  private val address = Play.current.configuration.getString("neo4j.address").get

  private val user = Play.current.configuration.getString("neo4j.user").get

  private val password = Play.current.configuration.getString("neo4j.password").get

  private val queryPath = "http://" + address + "/db/data/"

  private val transactionalEndpoint = "http://" + address + "/db/data/transaction/"

  /**
    * Util functions
    */

  private def withAuth (path: String): WSRequest =
    WS.url(path).withAuth(user, password, WSAuthScheme.BASIC)

  def reachable: Future[WSResponse] = for {
    response <- withAuth(queryPath).head()
    _ <- response statusMustBe 200
  } yield response

  implicit class StringUtils (str: String) {
    def trim: String =
      str.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "")

    def clean: String =
      str.replaceAll("""[^a-zA-Z0-9()\s?!¿¡]""", "")

    def escapeParenthesis: String =
      str.replaceAll("""\(""", """\\(""")
         .replaceAll("""\)""", """\\)""")
  }

  /**
    * Response complementary operations
    */

  implicit private class ResponseOps (response: WSResponse) {

    def future: Future[WSResponse] = Future(response)

    def statusMustBe (status: Int): Future[WSResponse] =
      if (response.status != status)
        throw StatusErr("Status had to be "+status.toString+" in response: " + response.toString)
      else
        response.future

    def deserialize[A](implicit reads: Reads[A]): Future[A] = Future(response.json.as[A])

    def checkForTransactionErrs: Future[WSResponse] = Future(
      (response.json \ "errors").as[Seq[DeserializedTxErr]] match {
        case errs =>
          if (errs.length > 0)
            throw TxErr(errs.toString)
          else
            response
      }
    )

    def transaction: Future[Transaction] = Future(response.json.as[Transaction] match {
      case tx => Transaction(tx.commit.reverse.drop(7).reverse, tx.commit, tx.expires)
    })

    def txResult: Future[TxResult] = {
      val results = (response.json \ "results").as[List[JsValue]]
      if (results.length > 0) {
        val rs = results.map { case result =>
          val columns = (result \ "columns").as[List[String]] zip Stream.from(0)
          val data = (result \ "data" \\ "row").map(_.as[List[JsValue]])
          (columns.map { case (col, i) =>
            (col -> (data.map { case json =>
              json(i).as[JsValue]
            }).toList)
          }).toMap
        }
        Future(rs)
      }
      else {
        Future(List.empty[Map[String, List[JsValue]]])
      }
    }
  }

  /**
    * Node
    */

  def getNode (id: Int): Future[Node] = for {
    response <- withAuth(queryPath + "node/" + id).get()
    _ <- response statusMustBe 200
    node <- response.deserialize[Node]
  } yield node

  def createNode (properties: JsValue): Future[Node] = for {
    response <- withAuth(queryPath + "node").post(properties)
    _ <- response statusMustBe 201
    node <- response.deserialize[Node]
  } yield node

  def createNode: Future[Node] = createNode(Json.obj())

  implicit class NodeOps (n: Node) {

    def future: Future[Node] = Future(n)

    def addLabel (label: String): Future[Node] = for {
        response <- withAuth(n.labels).post(JsString(label))
        _ <- response statusMustBe 204
      } yield n

    def setProp (prop: String, value: JsValue): Future[Node] = for {
      response <- withAuth(n.properties + "/" + prop).put(value)
      _ <- response statusMustBe 204
    } yield n

    def updateProp (props: JsValue): Future[Node] = for {
      response <- withAuth(n.properties).put(props)
      _ <- response statusMustBe 204
    } yield n

    def getProps: Future[JsValue] = for {
      response <- withAuth(n.properties).get()
      _ <- response statusMustBe 200
    } yield response.json

    def delete: Future[Node] = for {
      response <- withAuth(n.self).delete()
      _ <- response statusMustBe 204
    } yield n

    def relate (target: Node, relType: String, data: JsObject): Future[Relationship] = for {
      response <- withAuth(n.createRelationship).post(Json.obj(
        "to" -> target.self,
        "type" -> relType,
        "data" -> data
      ))
      _ <- response statusMustBe 201
      relationship <- response.deserialize[Relationship]
    } yield relationship

    def relate (target: Node, relType: String): Future[Relationship] =
      n relate(target, relType, Json.obj())
  }

  /**
    * Relationships
    */

  def getRelationship (id: Int): Future[Relationship] = for {
    response <- withAuth(queryPath + "relationship/" + id).get()
    _ <- response statusMustBe 200
    relationship <- response.deserialize[Relationship]
  } yield relationship

  implicit class RelationshipOps (r: Relationship) {

    def delete: Future[Relationship] = for {
      response <- withAuth(r.self).delete()
      _ <- response statusMustBe 204
    } yield r
  }

  /**
    * Transactions
    */

  def openTransaction: Future[Transaction] = for {
    response <- withAuth(transactionalEndpoint).post(Json.obj("statements" -> Json.arr()))
    _ <- response statusMustBe 201
    _ <- response.checkForTransactionErrs
    transaction <- response.transaction
  } yield transaction

  def query (statement: JsArray): Future[TxResult] = for {
    tx <- openTransaction
    result <- tx lastly statement
  } yield result

  def query (statement: JsObject): Future[TxResult] =
    query(Json.arr(statement))

  def query (statement: String): Future[TxResult] =
    query(Json.obj("statement" -> statement))

  implicit class TransactionOps (tx: Transaction) {

    def execute (statements: JsArray): Future[TxResult] = for {
      response <- withAuth(tx.self).post(Json.obj("statements" -> statements))
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
      result <- response.txResult
    } yield result

    def execute (statement: JsObject): Future[TxResult] =
      tx execute Json.arr(statement)

    def execute (statement: String): Future[TxResult] =
      tx execute Json.obj("statement" -> statement)

    def lastly (statements: JsArray): Future[TxResult] = for {
      response <- withAuth(tx.commit).post(Json.obj("statements" -> statements))
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
      result <- response.txResult
    } yield result

    def lastly (statement: JsObject): Future[TxResult] =
      tx lastly Json.arr(statement)

    def lastly (statement: String): Future[TxResult] =
      tx lastly Json.obj("statement" -> statement)

    def finish: Future[Unit] = for {
      _ <- tx lastly Json.arr()
    } yield Unit

    def rollback: Future[Unit] = for {
      response <- withAuth(tx.self).delete()
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
    } yield Unit
  }
}
