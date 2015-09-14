package decision

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.decisions._
import decision.system.{phi, pi, notPi, minInfluence}

package object user {

  trait User

  object UserTag extends Tag[User]("User")

  case class UserInfo (
    email: String,
    name: String
  )

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

  implicit class UserTagOps (userTag: Tag[User]) {

    def create: Future[Arcklet[User, JsObject]] =
      create(Json.obj())

    def create (props: JsObject): Future[Arcklet[User, JsObject]] = for {
      tx <- openTransaction
      url <- userTag createUrl tx
      _ <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+userTag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj(
            "url" -> url,
            "influence" -> phi
          ) ++ props)))
    } yield Arcklet(userTag, url, props)

    def createWith[P] (props: P)(implicit pw: Writes[P]): Future[Arcklet[User, P]] = for {
      tx <- openTransaction
      url <- userTag createUrl tx
      _ <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+userTag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj(
            "url" -> url,
            "influence" -> phi
        ) ++ Json.toJson(props).as[JsObject])))
    } yield Arcklet(userTag, url, props)

    def authenticate (email: String, password: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (n:"+userTag.str+" {email: {emailmatch}, password: {passmatch}}) OPTIONAL MATCH (n)-[r:PIN]-(s:Skillbook) RETURN n.url, n.name, s"),
        "parameters" -> Json.obj(
          "emailmatch" -> email,
          "passmatch" -> password
        )))
    } yield if (result(0)("n.url").length == 0)
        None
      else if (result(0)("s")(0) == JsNull) {
        Some(Json.obj(
          "url" -> result(0)("n.url")(0),
          "email" -> email,
          "name" -> result(0)("n.name")(0),
          "skillbooks" -> Json.arr()
        ))
      } else {
        Some(Json.obj(
          "url" -> result(0)("n.url")(0),
          "email" -> email,
          "name" -> result(0)("n.name")(0),
          "skillbooks" -> Json.toJson(result(0)("s"))
        ))
      }
  }

  implicit class UserArckletOps[P] (user: Arcklet[User, P]) {

    def infuseUser[B] (that: Arcklet[User, B]): Future[Unit] = influenciate(that, "INFUSES")

    def infuse[A, B] (that: Arcklet[A, B]): Future[Unit] = user relate(that, "INFUSES")

    def drainUser[B] (that: Arcklet[User, B]): Future[Unit] = influenciate(that, "DRAINS")

    def drain[A, B] (that: Arcklet[A, B]): Future[Unit] = user relate(that, "DRAINS")

    def propose (ident: String, description: String, args: JsObject)
      (implicit electionsTime: FiniteDuration, minVoters: Int, actorsystem: ActorSystem): Future[Arcklet[Decision, DecisionManifest]] = for {
      tx <- openTransaction
      manifest <- DecisionTag createWith(tx, DecisionManifest(ident, description))
      args <- DecisionArgsTag create(tx, args)
      _ <- manifest relate(args, tx, "WITH_ARGUMENTS")
      _ <- tx.finish
      _ <- Future(startElection(manifest, args))
    } yield manifest

    private def influenciate[B] (that: Arcklet[User, B], reltype: String): Future[Unit] = for {
      tx <- openTransaction
      result <- tx execute Json.obj(
        "statement" -> s"""MATCH (ua:${user.tag.str} {url: {uaUrl}}),(ub:${that.tag.str} {url: {ubUrl}}) RETURN ua.influence, ub.influence""",
        "parameters" -> Json.obj(
          "uaUrl" -> user.url,
          "ubUrl" -> that.url))
      influences <- Future((result(0)("ua.influence")(0).as[Int] -> result(0)("ub.influence")(0).as[Int]))
      _ <- (influences._1, reltype) match {
        case (inf, _) if inf <= minInfluence =>
          tx.rollback
        case (_, "INFUSES") =>
          for {
            _ <- setInfluences(tx, that, "INFUSES", pi(influences))
            _ <- tx.finish
          } yield Unit
        case (_, "DRAINS") =>
          for {
            _ <- setInfluences(tx, that, "DRAINS", notPi(influences))
            _ <- tx.finish
          } yield Unit
      }
    } yield Unit

    private def setInfluences[B] (tx: Transaction, that: Arcklet[User, B], reltype: String, vals: (Int, Int)): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> s"""MATCH (ua:${user.tag.str} {url: {uaUrl}}),(ub:${that.tag.str} {url: {ubUrl}}) SET ua.influence={uaInf}, ub.influence={ubInf} CREATE (ua)-[:$reltype]->(ub)""",
        "parameters" -> Json.obj(
          "uaUrl" -> user.url,
          "ubUrl" -> that.url,
          "uaInf" -> vals._1,
          "ubInf" -> vals._2))
    } yield Unit
  }
}
