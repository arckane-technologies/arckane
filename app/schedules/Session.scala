/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.schedules

import java.time._
import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import com.github.nscala_time.time.Imports._

import arckane.db.transaction._
import arckane.db.persistence._

/** Data types and type classes of Session. */
package object session {

  /** Session type. */
  trait Session

  /** Data type for Neo4j :Session tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SessionTag extends Tag[Session]("Session")

  /** Type class for the [[SessionTag]] data type. */
  implicit class SessionTagOps (tag: Tag[Session]) {

    def getDay (instant: Long): String = {
      val date = new DateTime(instant)
      date.getDayOfWeek() match {
        case 1 => "Monday"
        case 2 => "Tuesday"
        case 3 => "Wednesday"
        case 4 => "Thursday"
        case 5 => "Friday"
        case 6 => "Saturday"
        case 7 => "Sunday"
      }
    }

    def getDate (instant: Long): String = {
      val date = new DateTime(instant)
      date.getDayOfMonth() + " " + (date.getMonthOfYear match {
        case 1 => "January"
        case 2 => "February"
        case 3 => "March"
        case 4 => "April"
        case 5 => "May"
        case 6 => "June"
        case 7 => "July"
        case 8 => "August"
        case 9 => "September"
        case 10 => "October"
        case 11 => "November"
        case 12 => "December"
      })
    }

    def getTime (instant: Long): String = {
      val date = new DateTime(instant)
      val hour = date.getHourOfDay()
      val minutes = date.getMinuteOfHour()
      (if (hour > 12) hour - 12 else hour) + ":" +
      (if (minutes < 10) "0" + minutes else minutes) + " " +
      (if (hour < 12) "a.m." else "p.m.")
    }

    def getSessionInfo (sessionId: String, viewer: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (s:"+tag.str+" {url: {sessionid}})<-[:MENTORS]-(u:User) "
          + "RETURN s.session_date, s.length, s.current, s.limit, s.price, u.url, u.firstname, u.lastname, u.rating"),
        "parameters" -> Json.obj(
          "sessionid" -> sessionId
        )))
    } yield if (result("u.firstname").length == 0) {
      None
    } else {
      val res = result
      val sessionDate = res("s.session_date").head.as[Long]
      Some(Json.obj(
        "is_owner" -> (if (viewer == res("u.url").head.as[String]) {
          true
        } else {
          false
        }),
        "mentor" -> Json.obj(
          "name" -> (res("u.firstname").head.as[String] + " " + res("u.lastname").head.as[String]),
          "picture" -> JsNull,
          "rating" -> res("u.rating").head
        ),
        "date" -> Json.obj(
          "day" -> getDay(sessionDate),
          "date" -> getDate(sessionDate),
          "time" -> getTime(sessionDate),
          "length" -> (res("s.length").head.as[Float] + " hrs.")
        ),
        "booking" -> Json.obj(
          "price" -> (res("s.price").head.as[Float]),
          "current" -> (res("s.current").head.as[Int]),
          "limit" -> (res("s.limit").head.as[Int])
        )
      ))
    }

    def getSessionEditData (sessionId: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (s:"+tag.str+" {url: {sessionid}}) "
          + "RETURN "
          + "s.session_date, "
          + "s.creation_timestamp, "
          + "s.latitude, "
          + "s.longitude, "
          + "s.formatted_address, "
          + "s.location_name, "
          + "s.location_web, "
          + "s.length, "
          + "s.current, "
          + "s.limit, "
          + "s.price"),
        "parameters" -> Json.obj(
          "sessionid" -> sessionId
        )))
    } yield if (result("s.session_date").length == 0) {
      None
    } else {
      Some(Json.obj(
        "session_date" -> result("s.session_date").head.as[Long],
        "creation_timestamp" -> result("s.creation_timestamp").head.as[Long],
        "formatted_address" -> result("s.formatted_address").head.as[String],
        "location_name" -> result("s.location_name").head.as[String],
        "location_web" -> result("s.location_web").head.as[String],
        "latitude" -> result("s.latitude").head.as[Float],
        "longitude" -> result("s.longitude").head.as[Float],
        "length" -> result("s.length").head.as[Float],
        "price" -> result("s.price").head.as[Float],
        "current" -> result("s.current").head.as[Int],
        "limit" -> result("s.limit").head.as[Int]
      ))
    }

    def retriveMentorSessions (mentor: String): Future[JsArray] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {userid}})-[:MENTORS]->(s:"+tag.str+") "
          + "RETURN s.url"),
        "parameters" -> Json.obj(
          "userid" -> mentor
        )
      ))
    } yield result("s.url").map { x =>
      Json.obj("sessionId" -> x)
    }.foldLeft(JsArray()) { (acc, x) =>
      acc ++ Json.arr(x)
    }

    def createSession (mentor: String): Future[Arcklet[Session, JsObject]] = for {
      tx <- openTransaction
      arcklet <- tag.create(tx, Json.obj(
        "creation_timestamp" -> ZonedDateTime.now(ZoneOffset.UTC).toEpochSecond() * 1000,
        "session_date" -> ZonedDateTime.now(ZoneOffset.UTC).toEpochSecond() * 1000,
        "formatted_address" -> "San Francisco",
        "location_name" -> "Unset",
        "location_web" -> "",
        "latitude" -> 37.77493,
        "longitude" -> -122.41942,
        "length" -> 0,
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

    def deleteSession (sessionId: String, userId: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" ->
          ( "MATCH (s:Session {url: {sessionid}})<-[r:MENTORS]-(u:User {url: {userid}}) "
          + "DELETE s,r"),
        "parameters" -> Json.obj(
          "sessionid" -> sessionId,
          "userid" -> userId
        )
      ))
    } yield Unit

    def isOwner (user: String, session: String): Future[Boolean] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (s:"+tag.str+" {url: {sessionid}})<-[:MENTORS]-(u:User {url: {userid}}) "
          + "RETURN u"),
        "parameters" -> Json.obj(
          "sessionid" -> session,
          "userid" -> user
        )
      ))
    } yield if (result("u").length == 0) {
      false
    } else {
      true
    }
  }
}
