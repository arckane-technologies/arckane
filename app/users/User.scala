/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package users

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of User. */
package object user {

  /** User type. */
  trait User

  /** Data type for Neo4j :User tag. Specializes the more general type [[database.persistence.Tag]]. */
  object UserTag extends Tag[User]("User")

  /** Type class for the [[UserTag]] data type. */
  implicit class UserTagOps (tag: Tag[User]) {

    def authenticate (email: String, password: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {email: {emailmatch}, password: {passmatch}}) RETURN n.url, n.name"),
        "parameters" -> Json.obj(
          "emailmatch" -> email,
          "passmatch" -> password
        )))
    } yield if (result(0)("n.url").length == 0)
        None
      else {
        Some(Json.obj(
          "url" -> result(0)("n.url")(0),
          "email" -> email,
          "name" -> result(0)("n.name")(0)
        ))
      }
  }
}
