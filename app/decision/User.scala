package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.system.{phi, pi, notPi, minInfluence}

package object user {

  trait User
  object UserTag extends Tag[User]("User")
  case class UserInfo (
    email: String,
    name: String,
    surname: String = "",
    gender: String = "",
    day: Int = 0,
    month: Int = 0,
    year: Int = 0
  )

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

    def create: Future[Arcklet[User, JsObject]] =
      create(Json.obj())

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

    def createWith[P] (props: P)(implicit pw: Writes[P]): Future[Arcklet[User, P]] = for {
      tx <- openTransaction
      url <- tx createUrl userTag
      result <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+userTag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj(
            "url" -> url,
            "influence" -> phi
        ) ++ Json.toJson(props).as[JsObject]))
      )
    } yield Arcklet(userTag, url, props)
  }

  implicit class UserArckletOps[P] (user: Arcklet[User, P]) {

    def infuseUser[B] (that: Arcklet[User, B]): Future[Unit] = influenciate(that, "INFUSES")

    def infuse[A, B] (that: Arcklet[A, B]): Future[Unit] = user relate(that, "INFUSES")

    def drainUser[B] (that: Arcklet[User, B]): Future[Unit] = influenciate(that, "DRAINS")

    def drain[A, B] (that: Arcklet[A, B]): Future[Unit] = user relate(that, "DRAINS")

    //def propose (ident: String, args: JsObject): Future[Unit] =

    private def influenciate[B] (that: Arcklet[User, B], reltype: String): Future[Unit] = for {
      tx <- openTransaction
      result <- tx execute Json.obj(
        "statement" -> s"""MATCH (ua:${user.tag.str} {url: {uaUrl}}),(ub:${that.tag.str} {url: {ubUrl}}) RETURN ua.influence, ub.influence""",
        "parameters" -> Json.obj(
          "uaUrl" -> user.url,
          "ubUrl" -> that.url
        )
      )
      influences <- Future((result("ua.influence")(0).as[Int] -> result("ub.influence")(0).as[Int]))
      _ <-
        if (influences._1 > minInfluence)
          if (reltype == "INFUSES")
            for {
              _ <- setInfluences(tx, that, "INFUSES", pi(influences))
              _ <- tx.finish
            } yield Unit
          else
            for {
              _ <- setInfluences(tx, that, "DRAINS", notPi(influences))
              _ <- tx.finish
            } yield Unit
        else tx.rollback
    } yield Unit

    private def setInfluences[B] (tx: Transaction, that: Arcklet[User, B], reltype: String, vals: (Int, Int)): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> s"""MATCH (ua:${user.tag.str} {url: {uaUrl}}),(ub:${that.tag.str} {url: {ubUrl}}) SET ua.influence={uaInf}, ub.influence={ubInf} CREATE (ua)-[:$reltype]->(ub)""",
        "parameters" -> Json.obj(
          "uaUrl" -> user.url,
          "ubUrl" -> that.url,
          "uaInf" -> vals._1,
          "ubInf" -> vals._2
        )
      )
    } yield Unit
  }
}
