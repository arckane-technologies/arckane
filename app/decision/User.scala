package decision

import scala.concurrent.Future

import scalaz.{Validation, Success, Failure}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.Persistent
import utils.Entity
import utils.ValidationOps._

object UserOps {

  case class User (id: Int, node: Node, props: UserProps) extends Entity
  case class UserProps (url: String, email: String, password: String)

  implicit val userPropsWrites = new Writes[UserProps] {
    def writes(props: UserProps) = Json.obj(
      "url" -> props.url,
      "email" -> props.email,
      "password" -> props.password
    )
  }

  implicit val userWrites = new Writes[User] {
    def writes(user: User) = Json.obj(
      "id" -> user.id,
      "node" -> user.node,
      "props" -> user.props
    )
  }

  implicit val userPropsReads = (
    (JsPath \ "url").read[String] and
    (JsPath \ "email").read[String] and
    (JsPath \ "password").read[String]
  )(UserProps.apply _)

  implicit val userReads = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "node").read[Node] and
    (JsPath \ "props").read[UserProps]
  )(User.apply _)

  implicit object UserPersistent extends Persistent[User, UserProps] {
    val tag: String = "User"
    val entityWrites: Writes[UserProps] = userPropsWrites
    val entityReads: Reads[UserProps] = userPropsReads
    def instantiate (props: UserProps, node: Node) = User(node.id, node, props)
  }
}
