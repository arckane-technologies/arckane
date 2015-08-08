package decision

import scala.concurrent._

import scalaz.{Validation, Success, Failure}
import scalaz.Validation.FlatMap._

import play.api.Logger
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.Persistent
import utils.Entity

object UserOps {

  case class User (id: Int, node: Node, props: UserProps) extends Entity
  case class UserProps (email: String, password: String)

  implicit object UserPersistent extends Persistent[User, UserProps] {
    val tag: String = "User"
    val entityWrites: Writes[UserProps] = new Writes[UserProps] {
      def writes(props: UserProps) = Json.obj(
        "email" -> props.email,
        "password" -> props.password
      )
    }
    val entityReads: Reads[UserProps] = (
      (JsPath \ "email").read[String] and
      (JsPath \ "password").read[String]
    )(UserProps.apply _)
    def instantiate (props: UserProps, node: Node) = User(node.id, node, props)
  }
}
