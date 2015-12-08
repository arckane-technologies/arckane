/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package schedules

import java.time._
import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of Session. */
package object session {

  /** Session type. */
  trait Session

  /** Data type for Neo4j :Session tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SessionTag extends Tag[Session]("Session")

  /** Type class for the [[SessionTag]] data type. */
  implicit class SessionTagOps (tag: Tag[Session]) {

    def retriveMentorSessions (mentor: String): Future[JsArray] = for {
      response <- query(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {userid}})-[:MENTORS]->(s:"+tag.str+") "
          + "RETURN s.url"),
        "parameters" -> Json.obj(
          "userid" -> mentor
        )
      ))
    } yield response(0)("s.url").map { x =>
      Json.obj("sessionId" -> x)
    }.foldLeft(JsArray()) { (acc, x) =>
      acc ++ Json.arr(x)
    }

    //  this.data = {
    //    mentor: {
    //      name: "Francisco Aramburo",
    //      picture: "../assets/images/profile.jpg",
    //      rating: 4.5
    //    },
    //    date: {
    //      day: "Thursday 29",
    //      time: "5:00 p.m.",
    //      length: "2 hrs."
    //    },
    //    booking: {
    //      price: "24.50",
    //      current: 4,
    //      limit: 5
    //    },
    //    skills: [

    def createSession (mentor: String): Future[Arcklet[Session, JsObject]] = for {
      tx <- openTransaction
      arcklet <- tag.create(tx, Json.obj(
        "creation_timestamp" -> ZonedDateTime.now(ZoneOffset.UTC).toEpochSecond(),
        "session_date" -> 0,
        "length" -> "unset",
        "price" -> 0,
        "limit" -> 0,
        "current" -> 0,
        "listed" -> false
      ))
      _ <- tx.lastly(Json.obj(
        "statement" ->
          ( "MATCH (s:"+tag.str+" {url: {sessionid}}), (u:User {url: {userid}}) "
          + "CREATE (u)-[:MENTORS]->(s) "),
        "parameters" -> Json.obj(
          "sessionid" -> arcklet.url,
          "userid" -> mentor
        )))
    } yield arcklet

    def isOwner (user: String, session: String): Future[Boolean] = for {
      response <- query(Json.obj(
        "statement" ->
          ( "MATCH (s:"+tag.str+" {url: {sessionid}})<-[:MENTORS]-(u:User {url: {userid}}) "
          + "RETURN u"),
        "parameters" -> Json.obj(
          "sessionid" -> session,
          "userid" -> user
        )
      ))
    } yield if (response(0)("u").length == 0) {
      false
    } else {
      true
    }
  }
}
