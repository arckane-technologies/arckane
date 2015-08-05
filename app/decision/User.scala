package decision

import scala.concurrent._

import scalaz.{Validation, Success, Failure}
import scalaz.Validation.FlatMap._

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

  def createUser (username: String, password: String)(implicit influenceRatio: Int => Double): Future[Validation[Error, User]] = for {
    tx <- openTransaction
    countResult <- execute(tx, Json.arr(Json.obj("statement" -> "MATCH (n: Person) RETURN count(n)")))
    influence <- for {
      result <- countResult
      count <- Success(influenceRatio(result.head("count(n)").as[Int]))
    } yield count
    userResult <- execute(tx, Json.arr(Json.obj(
      "statement" -> "CREATE (n:Person {props}) RETURN n",
      "parameters" -> Json.obj(
        "props" -> Json.obj(
          "username" -> username,
          "password" -> password,
          "influence" -> influence
        )),
      "resultDataContents" -> Json.arr("REST")
    )))
    user <- Future(userResult match {
      case Success(result: TxResult) => (result.head("n") \ "rest").validate[Node] match {
        case s: JsSuccess[Node] => Success(new User {
            val node: Node = s.get
            val username: String = username
            val password: String = password
            val influence: Double = influence
        })
        case e: JsError =>
          val error = "JsError when trying to deserialize a Node: " + JsError.toJson(e).toString()
          Failure(error)
      }
      case e: Failure[Error] => e
    })
    _ <- commit(tx)
  } yield user

}
