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

  }
}
