package decision

import scala.concurrent._

import scalaz.{Validation, Success, Failure}
import scalaz.Validation.FlatMap._

import play.api.Logger
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import utils.Database._

sealed trait User {

  val node: Node

  val id: Int = node.id

  val username: String

  val password: String

  val influence: Double
}

object UserFunctions {

  def createUser (uname: String, upass: String)(implicit influenceRatio: Int => Double): Future[Validation[Error, User]] = for {
    tx <- openTransaction
    countResult <- executeUserBaseCount(tx)
    uinfl <- extractInfluence(countResult, influenceRatio)
    userResult <- executeUserCreation(tx, uname, upass, uinfl)
    unode <- extractUserNode(userResult)
    user <- instantiateUser(uname, upass, uinfl, unode)
    _ <- allOrNothing(tx, user)
  } yield user

  private def executeUserBaseCount (tx: Validation[Error, Transaction]): Future[Validation[Error, TxResult]] =
    execute(tx, Json.arr(Json.obj("statement" -> "MATCH (n: Person) RETURN count(n)")))

  private def extractInfluence (tx: Validation[Error, TxResult], influenceRatio: Int => Double): Future[Validation[Error, Double]] = tx match {
    case Success(result) => Future(Success(influenceRatio(result.head("count(n)").as[Int])))
    case e: Failure[Error] => Future(e)
  }

  private def executeUserCreation (
    tx: Validation[Error, Transaction],
    uname: String,
    upass: String,
    uinfl: Validation[Error, Double]
  ): Future[Validation[Error, TxResult]] = uinfl match {
    case Success(influence) => execute(tx, Json.arr(Json.obj(
      "statement" -> "CREATE (n:Person {props}) RETURN n",
      "parameters" -> Json.obj(
        "props" -> Json.obj(
          "username" -> uname,
          "password" -> upass,
          "influence" -> influence
        )),
      "resultDataContents" -> Json.arr("REST")
    )))
    case e: Failure[Error] => Future(e)
  }

  private def extractUserNode (tx: Validation[Error, TxResult]): Future[Validation[Error, Node]] = tx match {
    case Success(result: TxResult) => (result.head("n") \ "rest").validate[Node] match {
      case s: JsSuccess[Node] =>
        Future(Success(s.get))
      case e: JsError =>
        val error = "JsError when trying to deserialize a Node: " + JsError.toJson(e).toString()
        Logger.error(error)
        Future(Failure(error))
    }
    case e: Failure[Error] => Future(e)
  }

  private def instantiateUser (
    uname: String,
    upass: String,
    uinfl: Validation[Error, Double],
    unode: Validation[Error, Node]
  ): Future[Validation[Error, User]] = (unode, uinfl) match {
    case (Success(userNode), Success(userInfluence)) => Future(Success(new User {
      val node: Node = userNode
      val username: String = uname
      val password: String = upass
      val influence: Double = userInfluence
    }))
    case (e: Failure[Error], _) =>
      Future(e)
    case (_, e: Failure[Error]) =>
      Future(e)
  }
}
