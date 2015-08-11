package decision

import scala.concurrent.Future

import scalaz.{Validation, Success, Failure}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.ValidationOps._
import utils.Tagged
import utils.Persistent
import utils.{Entity, EntityProps}

object UserOps {

  case class User (props: EntityProps, url: String, node: Node) extends Entity

  implicit val userTag = new Tagged[User] {
    val tag: String = "User"
  }

  case class UserBasicInfo (email: String) extends EntityProps

  implicit val userBasicInfoWrites = new Writes[UserBasicInfo] {
    def writes(props: UserBasicInfo) = Json.obj(
      "email" -> props.email
    )
  }

  implicit val userBasicInfoReads =
    (__ \ "email").read[String].map(v => UserBasicInfo(v))
  //(
  //  (JsPath \ "url").read[String] and
  //  (JsPath \ "email").read[String]
  //)(UserBasicInfo.apply _)

  implicit object UserBasicInfoPersistence extends Persistent[User, UserBasicInfo] {
    def instantiate (props: UserBasicInfo, url: String, node: Node) = User(props, url, node)
  }

  case class UserPassword (password: String) extends EntityProps

  implicit val userPasswordWrites = new Writes[UserPassword] {
    def writes(props: UserPassword) = Json.obj(
      "password" -> props.password
    )
  }

  implicit val userPasswordReads =
    (__ \ "password").read[String].map(v => UserPassword(v))

  implicit object UserPasswordPersistence extends Persistent[User, UserPassword] {
    def instantiate (props: UserPassword, url: String, node: Node) = User(props, url, node)
  }

  /*
   * User Ops.
   */

  def infuse (user: User, entity: Entity): Future[Validation[Err, Relationship]] = for {
    relationship <- createRelationship(user.node, entity.node, "INFUSES")
  } yield relationship
}
