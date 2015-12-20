/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.db

import scala.concurrent.Future

import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.neo4j._
import arckane.db.txresponse._

package object transaction {

  /* First list is for results of every cypher query, then a map for each match and their list of value. */
  type TxResult = Map[String, List[JsValue]]

  /** Data type for neo4j transaction errors.
    *
    * @see [[http://neo4j.com/docs/stable/rest-api-transactional.html]] for Neo4j cypher transactional endpoint docs.
    */
  case class DeserializedTxErr (code: String, message: Option[String])

  /** RuntimeException for transaction error. */
  case class TxErr (message: String) extends RuntimeException(message)

  /** RESTful API endpoint for Neo4j's cypher transactional endpoint. */
  private val transactionalEndpoint = address + "/db/data/transaction"

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

  /** Generic request to the transactional end point.
    *
    * @param statements array of json objects with parameters and queries to be executed.
    * @return [[WSResponse]] of the request.
    */
  def transactionRequest (path: String, statements: JsArray): Future[WSResponse] = for {
    response <- withAuth(path).post(Json.obj("statements" -> statements))
    _ <- response statusMustBe 200
    _ <- response.checkForTransactionErrs
  } yield response

  /** Creates a transaction, sends several cypher queries, commits the transaction and
    * retrives the list of results from all the queries.
    *
    * @param statements array of json objects with parameters and queries to be executed.
    * @return the list of [[TxResult]] for the executed query wraped in a Future.
    */
  def query (statements: JsArray): Future[List[TxResult]] =
    transactionRequest(
      transactionalEndpoint+"/commit",
      statements
    ).flatMap(_.allResults)

  /** Creates a transaction, sends a cypher query, commits the transaction and
    * returns the unique result.
    *
    * @param statement json object with the query and parameters to be executed.
    * @return the [[TxResult]] of the executed query wraped in a Future.
    */
  def query (statement: JsObject): Future[TxResult] =
    transactionRequest(
      transactionalEndpoint+"/commit",
      Json.arr(statement)
    ).flatMap(_.result)

  /** Creates a transaction, sends a cypher query, commits the transaction and
    * returns the unique result.
    *
    * @param statement query string to be executed.
    * @return the [[TxResult]] of the executed query wraped in a Future.
    */
  def query (statement: String): Future[TxResult] =
    query(Json.obj("statement" -> statement))

  /** Data type for neo4j transactions.
    *
    * @see [[http://neo4j.com/docs/stable/rest-api-transactional.html]] for Neo4j cypher transactional endpoint docs.
    */
  case class Transaction (self: String, commit: String, expires: String) { tx =>

    /** Sends several cypher queries with this transaction without commiting.
      *
      * @param statement array of json objects with parameters and queries to be executed.
      * @return the list of [[TxResult]] for the executed query wraped in a Future.
      */
    def execute (statements: JsArray): Future[List[TxResult]] =
      transactionRequest(
        tx.self,
        statements
      ).flatMap(_.allResults)

    /** Sends a json object with parameters and a query with this transaction without commiting.
      *
      * @param statement json object with the query and parameters to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def execute (statement: JsObject): Future[TxResult] =
      transactionRequest(
        tx.self,
        Json.arr(statement)
      ).flatMap(_.result)

    /** Sends a query string with this transaction without commiting.
      *
      * @param statement query string to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def execute (statement: String): Future[TxResult] =
      execute(Json.obj("statement" -> statement))

    /** Sends several cypher queries with this transaction and commiting in the end.
      *
      * @param statement array of json objects with parameters and queries to be executed.
      * @return the list of [[TxResult]] for the executed query wraped in a Future.
      */
    def lastly (statements: JsArray): Future[List[TxResult]] =
      transactionRequest(
        tx.commit,
        statements
      ).flatMap(_.allResults)

    /** Sends a json object with parameters and a query with this transaction and commiting in the end.
      *
      * @param statement json object with the query and parameters to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def lastly (statement: JsObject): Future[TxResult] =
      transactionRequest(
        tx.commit,
        Json.arr(statement)
      ).flatMap(_.result)

    /** Sends a query string with this transaction and commiting in the end.
      *
      * @param statement query string to be executed.
      * @return the [[TxResult]] of the executed query wraped in a Future.
      */
    def lastly (statement: String): Future[TxResult] =
      lastly(Json.obj("statement" -> statement))

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

  /** Used to transform objects from json to its equivalent [[Transaction]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
  implicit val transactionReads: Reads[Transaction] = (
    (JsPath \ "commit").read[String] and
    (JsPath \ "commit").read[String] and
    (JsPath \ "transaction" \ "expires").read[String]
  )(Transaction.apply _)

  /** Used to transform objects from json to its equivalent [[DeserializedTxErr]] data type.
    *
    * @see [[https://www.playframework.com/documentation/2.4.x/ScalaJson#JsValue-to-a-model]] for Play Framework's Reader docs.
    */
  implicit val transactionErrReads: Reads[DeserializedTxErr] = (
      (JsPath \ "code").read[String] and
      (JsPath \ "message").readNullable[String]
  )(DeserializedTxErr.apply _)
}
