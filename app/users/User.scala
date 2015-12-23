/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.users

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.Node
import arckane.db.transaction._

package object user {

  def userCreate (firstname: String, lastname: String, email: String, password: String): Future[String] = for {
    uri <- Node.create(Json.obj(
      "firstname" -> firstname,
      "lastname" -> lastname,
      "email" -> email,
      "password" -> password
    ))("User")
  } yield uri

  /** Authenticates the user, returns a json with basic info. */
  def authenticate (email: String, password: String): Future[Option[(String, String)]] = for {
    result <- query(Json.obj(
      "statement" -> """MATCH (n:User {email: {emailmatch}, password: {passmatch}})
                        RETURN n.uri, n.firstname""",
      "parameters" -> Json.obj(
        "emailmatch" -> email,
        "passmatch" -> password
      )))
  } yield {
    result("n.uri").length match {
      case 0 => None
      case _ => Some((
        result("n.uri").head.as[String],
        result("n.firstname").head.as[String]
      ))
    }
  }
}
