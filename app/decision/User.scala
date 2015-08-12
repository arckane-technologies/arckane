package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._

package object user {

  trait User
  object UserTag extends Tag[User]("User")
  case class UserInfo (email: String, name: String)

  implicit val userInfoWrites = new Writes[UserInfo] {
    def writes(props: UserInfo) = Json.obj(
      "email" -> props.email,
      "name" -> props.name
    )
  }

  implicit val userInfoReads = (
    (JsPath \ "email").read[String] and
    (JsPath \ "name").read[String]
  )(UserInfo.apply _)

  /*
   * User Ops.
   */

  implicit class UserArckletOps[P] (user: Arcklet[User, P]) {

    def infuse[A, B](that: Arcklet[A, B]): Future[Unit] = user relate(that, "INFUSES")
  }
}
