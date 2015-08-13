package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.system.{phi, pi, alpha}

package object user {

  trait User
  object UserTag extends Tag[User]("User")
  case class UserInfo (email: String, name: String, surname: String, gender: String, day: Int, month: Int, year: Int)

  implicit val userInfoWrites = new Writes[UserInfo] {
    def writes(props: UserInfo) = Json.obj(
      "email" -> props.email,
      "name" -> props.name,
      "surname" -> props.surname,
      "gender" -> props.gender,
      "day" -> props.day,
      "month" -> props.month,
      "year" -> props.year
    )
  }

  implicit val userInfoReads = (
    (JsPath \ "email").read[String] and
    (JsPath \ "name").read[String] and
    (JsPath \ "surname").read[String] and
    (JsPath \ "gender").read[String] and
    (JsPath \ "day").read[Int] and
    (JsPath \ "month").read[Int] and
    (JsPath \ "year").read[Int]
  )(UserInfo.apply _)

  /*
   * User Ops.
   */

  implicit class UserTagOps (userTag: Tag[User]) {

    def create (props: JsObject): Future[Arcklet[User, JsObject]] = for {
      tx <- openTransaction
      url <- tx createUrl userTag
      result <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+userTag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj(
            "url" -> url,
            "influence" -> phi
          ) ++ props)
        )
      )
    } yield Arcklet(userTag, url, props)
  }

  implicit class UserArckletOps[P] (user: Arcklet[User, P]) {

    def infuse[A, B](that: Arcklet[A, B]): Future[Unit] = user relate(that, "INFUSES")
    def drain[A, B](that: Arcklet[A, B]): Future[Unit] = user relate(that, "DRAINS")
  }
}
