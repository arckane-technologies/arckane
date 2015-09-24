/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package database

import scala.concurrent.Future

import play.api._
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.libs.json._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._

/** Functions, data types and type classes to interact easily with Neo4j. The
  * interaction is made using Neo4j's RESTful API. Most of the API is abstracted
  * through this module, but is preferable to mainly use the cypher transactional
  * endpoint.
  *
  * @see [[http://neo4j.com/docs/stable/rest-api.html]] documentation for Neo4j's REST API.
  */
package object neo4j {

  /** Node data type, used mainly for the non-(transactional cypher HTTP endpoint)
    * operations.
    */
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

  /** Used to transform objects from [[Node]] data type to its equivalent json object.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#Using-Writes-converters]] for Play Framework's Writer docs.
    */
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

  /** Used to transform objects from json to its equivalent [[Node]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
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

  /** Relationship data type, used mainly for the non-(transactional cypher HTTP endpoint)
    * operations.
    */
  case class Relationship (
    id: Int,
    start: String,
    property: String,
    self: String,
    properties: String,
    rtype: String,
    end: String
  )

  /** Used to transform objects from [[Relationship]] data type to its equivalent json object.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#Using-Writes-converters]] for Play Framework's Writer docs.
    */
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

  /** Used to transform objects from json to its equivalent [[Relationship]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
  implicit val relationshipReads: Reads[Relationship] = (
    (JsPath \ "metadata" \ "id").read[Int] and
    (JsPath \ "start").read[String] and
    (JsPath \ "property").read[String] and
    (JsPath \ "self").read[String] and
    (JsPath \ "properties").read[String] and
    (JsPath \ "type").read[String] and
    (JsPath \ "end").read[String]
  )(Relationship.apply _)

  /** Data type for neo4j transactions, used mainly for the cypher transactional endpoint.
    *
    * @see [[http://neo4j.com/docs/stable/rest-api-transactional.html]] for Neo4j cypher transactional endpoint docs.
    */
  case class Transaction (self: String, commit: String, expires: String)

  /** Used to transform objects from json to its equivalent [[Transaction]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
  private implicit val transactionReads: Reads[Transaction] = (
    (JsPath \ "commit").read[String] and
    (JsPath \ "commit").read[String] and
    (JsPath \ "transaction" \ "expires").read[String]
  )(Transaction.apply _)

  /** Data type for neo4j transaction errors, used mainly for the cypher transactional endpoint.
    *
    * @see [[http://neo4j.com/docs/stable/rest-api-transactional.html]] for Neo4j cypher transactional endpoint docs.
    */
  case class DeserializedTxErr (code: String, message: Option[String])

  /** Used to transform objects from json to its equivalent [[DeserializedTxErr]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
  private implicit val transactionErrReads: Reads[DeserializedTxErr] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "message").readNullable[String]
  )(DeserializedTxErr.apply _)

  /** RuntimeException for transaction error. */
  case class TxErr (message: String) extends RuntimeException(message)

  /** RuntimeException for errors when deserializing json. */
  case class DeserializationErr (message: String) extends RuntimeException(message)

  /** RuntimeException for non-expected http response status. */
  case class StatusErr (message: String) extends RuntimeException(message)

  /* First list is for results of every cypher query, then a map for each match and their list of value. */
  type TxResult = List[Map[String, List[JsValue]]]

  /** Neo4j addess taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_ADDRESS environment variable in the system. */
  private val address = Play.current.configuration.getString("neo4j.address").get

  /** Neo4j user taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_USER environment variable in the system. */
  private val user = Play.current.configuration.getString("neo4j.user").get

  /** Neo4j password taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_PASSWORD environment variable in the system. */
  private val password = Play.current.configuration.getString("neo4j.password").get

  /** RESTful API endpoint of Neo4j. */
  private val queryPath = "http://" + address + "/db/data/"

  /** RESTful API endpoint for Neo4j's cypher transactional endpoint. */
  private val transactionalEndpoint = "http://" + address + "/db/data/transaction/"

  /**
    * Util functions
    */

  /** Creates a Play Framework's WSRequest with added authentication.
    *
    * @param path url to where the request is going to be made.
    * @return a WSRequest object with authentication added, taken from the user
    * and password values.
    */
  private def withAuth (path: String): WSRequest =
    WS.url(path).withAuth(user, password, WSAuthScheme.BASIC)

  /** Sends a request HEAD to the Neo4j server to check for reachability and checks
    * that the response status is 200.
    *
    * @return a WSResponse result from the HEAD request.
    */
  def reachable: Future[WSResponse] = for {
    response <- withAuth(queryPath).head()
    _ <- response statusMustBe 200
  } yield response

  /** Type class that extends string methods for utility. */
  implicit class StringUtils (str: String) {

    /** Removes spaces and new line characters at the beginning and end of the string. */
    def trim: String =
      str.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "").replaceAll("""(\s)+""", " ")

    /** Removes all characters that are not letters, numbers, spaces, interrogation
      * and exclamation symbols. */
    def clean: String =
      str.replaceAll("""[^a-zA-Z0-9()\s?!¿¡.,]""", "")

    /** Adds backslashes to parenthesis to escape them for regexp searches. */
    def escapeParenthesis: String =
      str.replaceAll("""\(""", """\\(""")
         .replaceAll("""\)""", """\\)""")

    /** Capitalizes each word of the string. */
    def capitalizeWords: String =
      str.split(" ").foldLeft("")(_ + " " + _.capitalize).tail
  }

  /** Type class that adds methods to Play Framework's WSResponse type. */
  implicit private class ResponseOps (response: WSResponse) {

    /** Raises a future from a WSResponse. */
    def future: Future[WSResponse] = Future(response)

    /** Throws an [[StatusErr]] if the WSResponse has a different status code than
      * the expected one.
      *
      * @param status expected.
      * @return the same WSResponse in a Future.
      */
    def statusMustBe (status: Int): Future[WSResponse] =
      if (response.status != status)
        throw StatusErr("Status had to be "+status.toString+" in response: " + response.toString)
      else
        response.future

    /** Deserializes the json in the WSResponse by converting it to a data type.
      *
      * @param reads implicit, a Reads Play Framework json converter, normally
      * obtained by importing a package that has declared the implicit reads.
      * @return the converted json as the data type inside a Future.
      */
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

    /** Creates a [[Transaction]] data type out of the json of the WSResponse. */
    def transaction: Future[Transaction] = Future(response.json.as[Transaction] match {
      case tx => Transaction(tx.commit.reverse.drop(7).reverse, tx.commit, tx.expires)
    })

    /** Creates a [[TxResult]] data type out of the response json. Check the
      * cypher transactional endpoint documentation to see the structure of the result.
      */
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

  /** Returns the [[Node]] that matches the Neo4j generated id.
    * Warning: Neo4j ids are changed each time the database is restarted.
    *
    * @param id to look for.
    * @return a [[Node]] wraped in a Future.
    */
  def getNode (id: Int): Future[Node] = for {
    response <- withAuth(queryPath + "node/" + id).get()
    _ <- response statusMustBe 200
    node <- response.deserialize[Node]
  } yield node

  /** Creates a [[Node]] and saves it to the database.
    *
    * @param properties that will be saved to the database.
    * @return a [[Node]] wraped in a Future.
    */
  def createNode (properties: JsValue): Future[Node] = for {
    response <- withAuth(queryPath + "node").post(properties)
    _ <- response statusMustBe 201
    node <- response.deserialize[Node]
  } yield node

  /** Creates a [[Node]] ad saves it to the database without properties.
    *
    * @return a [[Node]] wraped in a Future.
    */
  def createNode: Future[Node] = createNode(Json.obj())

  /** Type class for the [[Node]] data type. */
  implicit class NodeOps (n: Node) {

    /** Raises the [[Node]] into a Future. */
    def future: Future[Node] = Future(n)

    /** Adds a Neo4j label to this node.
      *
      * @param label to be added to the node.
      * @return this same Node.
      */
    def addLabel (label: String): Future[Node] = for {
        response <- withAuth(n.labels).post(JsString(label))
        _ <- response statusMustBe 204
      } yield n

    /** Sets a Neo4j property to this node.
      *
      * @param property name to be set.
      * @param value of the property to be set.
      * @return this same Node.
      */
    def setProp (property: String, value: JsValue): Future[Node] = for {
      response <- withAuth(n.properties + "/" + property).put(value)
      _ <- response statusMustBe 204
    } yield n

    /** Updates a property or properties of the node in the database.
      *
      * @param props json object with the properties to be updated.
      * @return this same Node.
      */
    def updateProp (props: JsValue): Future[Node] = for {
      response <- withAuth(n.properties).put(props)
      _ <- response statusMustBe 204
    } yield n

    /** Retrives the properties of the node from the database.
      *
      * @return the json object with all the database properties of the node.
      */
    def getProps: Future[JsValue] = for {
      response <- withAuth(n.properties).get()
      _ <- response statusMustBe 200
    } yield response.json

    /** Deletes this node from the databse (not included relationships).
      *
      * @return this same Node.
      */
    def delete: Future[Node] = for {
      response <- withAuth(n.self).delete()
      _ <- response statusMustBe 204
    } yield n

    /** Creates a relationship between an other [[Node]] and this node.
      *
      * @param target Node to be related to.
      * @param relType tag of the relationship.
      * @param data attributes or properties to be added to the relationship.
      * @return the [[Relationship]] data type created wraped in a Future.
      */
    def relate (target: Node, relType: String, data: JsObject): Future[Relationship] = for {
      response <- withAuth(n.createRelationship).post(Json.obj(
        "to" -> target.self,
        "type" -> relType,
        "data" -> data
      ))
      _ <- response statusMustBe 201
      relationship <- response.deserialize[Relationship]
    } yield relationship

    /** Creates a relationship between an other [[Node]] and this node without
      * relationship attributes.
      *
      * @param target Node to be related to.
      * @param relType tag of the relationship.
      * @return the [[Relationship]] data type created wraped in a Future.
      */
    def relate (target: Node, relType: String): Future[Relationship] =
      n relate(target, relType, Json.obj())
  }

  /**
    * Relationships
    */

  /** Returns the [[Relationship]] that matches the Neo4j generated id.
    * Warning: Neo4j ids are changed each time the database is restarted.
    *
    * @param id to look for.
    * @return a [[Relationship]] wraped in a Future.
    */
  def getRelationship (id: Int): Future[Relationship] = for {
    response <- withAuth(queryPath + "relationship/" + id).get()
    _ <- response statusMustBe 200
    relationship <- response.deserialize[Relationship]
  } yield relationship

  /** Type class for the [[Relationship]] data type. */
  implicit class RelationshipOps (r: Relationship) {

    /** Deletes this relationship from the databse.
      *
      * @return this same Relationship.
      */
    def delete: Future[Relationship] = for {
      response <- withAuth(r.self).delete()
      _ <- response statusMustBe 204
    } yield r
  }

  /**
    * Transactions
    */

  /** Creates a new Neo4j cypher transaction endpoint [[Transaction]].
    * Throws any RuntimeException that may occur in the process.
    *
    * @return the created [[Transaction]]
    */
  def openTransaction: Future[Transaction] = for {
    response <- withAuth(transactionalEndpoint).post(Json.obj("statements" -> Json.arr()))
    _ <- response statusMustBe 201
    _ <- response.checkForTransactionErrs
    transaction <- response.transaction
  } yield transaction

  /** Creates a transaction, sends several cypher queries and commits the transaction.
    *
    * @param statement array of json objects with parameters and queries to be executed.
    * @return the [[TxResult]] of the executed query wraped in a Future.
    */
  def query (statement: JsArray): Future[TxResult] = for {
    tx <- openTransaction
    result <- tx lastly statement
  } yield result

  /** Creates a transaction, sends a cypher query and commits the transaction.
    *
    * @param statement json object with the query and parameters to be executed.
    * @return the [[TxResult]] of the executed query wraped in a Future.
    */
  def query (statement: JsObject): Future[TxResult] =
    query(Json.arr(statement))

  /** Creates a transaction, sends a cypher query and commits the transaction.
    *
    * @param statement query string to be executed.
    * @return the [[TxResult]] of the executed query wraped in a Future.
    */
  def query (statement: String): Future[TxResult] =
    query(Json.obj("statement" -> statement))

  /** Type class with all operations for an open transaction. */
  implicit class TransactionOps (tx: Transaction) {

    /** Sends several cypher queries with this transaction without commiting.
      *
      * @param statement array of json objects with parameters and queries to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def execute (statements: JsArray): Future[TxResult] = for {
      response <- withAuth(tx.self).post(Json.obj("statements" -> statements))
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
      result <- response.txResult
    } yield result

    /** Sends a json object with parameters and a query with this transaction without commiting.
      *
      * @param statement json object with the query and parameters to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def execute (statement: JsObject): Future[TxResult] =
      tx execute Json.arr(statement)

    /** Sends a query string with this transaction without commiting.
      *
      * @param statement query string to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def execute (statement: String): Future[TxResult] =
      tx execute Json.obj("statement" -> statement)

    /** Sends several cypher queries with this transaction and commiting in the end.
      *
      * @param statement array of json objects with parameters and queries to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def lastly (statements: JsArray): Future[TxResult] = for {
      response <- withAuth(tx.commit).post(Json.obj("statements" -> statements))
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
      result <- response.txResult
    } yield result

    /** Sends a json object with parameters and a query with this transaction and commiting in the end.
      *
      * @param statement json object with the query and parameters to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def lastly (statement: JsObject): Future[TxResult] =
      tx lastly Json.arr(statement)

    /** Sends a query string with this transaction and commiting in the end.
      *
      * @param statement query string to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def lastly (statement: String): Future[TxResult] =
      tx lastly Json.obj("statement" -> statement)

    /** Commits this transaction. */
    def finish: Future[Unit] = for {
      _ <- tx lastly Json.arr()
    } yield Unit

    /** Ends the transaction but orders the database to not execute the queries. */
    def rollback: Future[Unit] = for {
      response <- withAuth(tx.self).delete()
      _ <- response statusMustBe 200
      _ <- response.checkForTransactionErrs
    } yield Unit
  }
}
