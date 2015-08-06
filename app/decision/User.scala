package decision

import scala.concurrent._

import scalaz.{Validation, Success, Failure}
import scalaz.Validation.FlatMap._

import play.api.Logger
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.ValidationOps._

object UserOps {

  case class User (id: Int, node: Node, email: String, password: String, influence: Double)

  def createUser (
    umail: String,
    upass: String
  )(implicit decisionSystem: DecisionSystem): Future[Validation[Err, User]] = for {
    tx <- openTransaction
    countResult <- executeUserBaseCount(tx)
    uinfl <- extractInfluence(countResult, decisionSystem.influenceRatio)
    userResult <- executeUserCreation(tx, umail, upass, uinfl)
    unode <- extractUserNode(userResult)
    user <- instantiateUser(umail, upass, uinfl, unode)
    _ <- allOrNothing(tx, user)
  } yield user

  def getUser (id: Int): Future[Validation[Err, User]] = for {
    node <- getNode(id)
    props <- getNodeProperties(node)
    user <- instantiateUser(node, props)
  } yield user

  def deleteUser (user: Validation[Err, User]): Future[Validation[Err, TxResult]] =
    ifSucceeds(user) { user: User =>
      for {
        tx <- openTransaction
        deleteResult <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n:Person) WHERE id(n)=" + user.id.toString + " DELETE n"))))
        _ <- allOrNothing(tx, deleteResult)
      } yield deleteResult
    }

  def deleteAllUserRelationships (user: Validation[Err, User]): Future[Validation[Err, TxResult]] =
    ifSucceeds(user) { user: User =>
      for {
        tx <- openTransaction
        deleteResult <- execute(tx, Json.arr(Json.obj("statement" -> ("MATCH (n:Person)-[r]-() WHERE id(n)=" + user.id.toString + " DELETE r"))))
        _ <- allOrNothing(tx, deleteResult)
      } yield deleteResult
    }

  private def executeUserBaseCount (tx: Validation[Err, Transaction]): Future[Validation[Err, TxResult]] =
    execute(tx, Json.arr(Json.obj("statement" -> "MATCH (n: Person) RETURN count(n)")))

  private def extractInfluence (tx: Validation[Err, TxResult], influenceRatio: Int => Double): Future[Validation[Err, Double]] =
    ifSucceeds(tx) { result: TxResult =>
      Future(Success(influenceRatio((result.head("count(n)") \ "row")(0).as[Int])))
    }

  private def executeUserCreation (
    tx: Validation[Err, Transaction],
    umail: String,
    upass: String,
    uinfl: Validation[Err, Double]
  ): Future[Validation[Err, TxResult]] = ifSucceeds(uinfl) { influence: Double =>
    execute(tx, Json.arr(Json.obj(
      "statement" -> "CREATE (n:Person {props}) RETURN n",
      "parameters" -> Json.obj(
        "props" -> Json.obj(
          "email" -> umail,
          "password" -> upass,
          "influence" -> influence
        )),
      "resultDataContents" -> Json.arr("REST")
    )))
  }

  private def extractUserNode (tx: Validation[Err, TxResult]): Future[Validation[Err, Node]] =
    ifSucceeds(tx) { result: TxResult =>
      (result.head("n") \ "rest")(0).validate[Node] match {
        case s: JsSuccess[Node] =>
          Future(Success(s.get))
        case e: JsError =>
          val error = DeserializationErr("JsError when trying to deserialize a Node: " + JsError.toJson(e).toString())
          Logger.error(error.toString)
          Future(Failure(error))
      }
    }

  private def instantiateUser (
    umail: String,
    upass: String,
    uinfl: Validation[Err, Double],
    unode: Validation[Err, Node]
  ): Future[Validation[Err, User]] = ifSucceeds(unode, uinfl) { (userNode: Node, userInfluence: Double) =>
    Future(Success(User(
      userNode.id,
      userNode,
      umail,
      upass,
      userInfluence
    )))
  }

  private def instantiateUser (
    node: Validation[Err, Node],
    props: Validation[Err, JsValue]
  ): Future[Validation[Err, User]] = ifSucceeds(node, props) { (node: Node, props: JsValue) =>
    Future(Success(User(
      node.id,
      node,
      (props \ "email").as[String],
      (props \ "password").as[String],
      (props \ "influence").as[Double]
    )))
  }
}
