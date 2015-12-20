/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.users

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._

package object user {

  /** Authenticates the user, returns a json with basic info. */
  def authenticate (email: String, password: String): Future[Option[JsObject]] = for {
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
      case _ => Some(Json.obj(
        "uri" -> result("n.uri").head,
        "email" -> email,
        "name" -> result("n.firstname").head
      ))
    }
  }
}
