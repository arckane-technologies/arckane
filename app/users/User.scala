/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.users

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import org.mindrot.jbcrypt.BCrypt

import arckane.db.Node
import arckane.db.transaction._

package object user {

  def userCreate (firstname: String, lastname: String, email: String, password: String): Future[String] = for {
    uri <- Node.create(Json.obj(
      "firstname" -> firstname,
      "lastname" -> lastname,
      "email" -> email,
      "password" -> BCrypt.hashpw(password, BCrypt.gensalt()),
      "timestamp" -> System.currentTimeMillis
    ))("User")
  } yield uri

  /** Authenticates the user, returns a json with basic info. */
  def authenticate (email: String, password: String): Future[Option[(String, String)]] = for {
    result <- query(Json.obj(
      "statement" -> """MATCH (n:User {email: {emailmatch}})
                        RETURN n.uri, n.firstname, n.password""",
      "parameters" -> Json.obj(
        "emailmatch" -> email
      )))
  } yield {
    (result("n.uri").length > 0 && BCrypt.checkpw(password, result("n.password").head.as[String])) match {
      case false => None
      case true => Some((
        result("n.uri").head.as[String],
        result("n.firstname").head.as[String]
      ))
    }
  }

  def userDelete (uri: String): Future[Unit] =
    Node.delete(uri)

  def userEmailTaken (email: String): Future[Boolean] = for {
    result <- query(Json.obj(
      "statement" -> """MATCH (n:User {email: {emailmatch}})
                        RETURN count(n)""",
      "parameters" -> Json.obj(
        "emailmatch" -> email
      )))
  } yield result("count(n)").head.as[Int] > 0
}
