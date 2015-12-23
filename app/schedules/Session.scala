/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.schedules

import java.time._
import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.Node
import arckane.db.transaction._

/** Data types and type classes of Session. */
package object session {

  def sessionCreate (mentor: String): Future[String] = for {
    tx <- openTransaction
    uri <- Node.create(tx, Json.obj(
      "creation_timestamp" -> System.currentTimeMillis,
      "session_date" -> System.currentTimeMillis,
      "formatted_address" -> "Unset",
      "location_name" -> "Unset",
      "location_web" -> "",
      "latitude" -> 37.77493,
      "longitude" -> -122.41942,
      "length" -> 0,
      "price" -> 0,
      "limit" -> 0,
      "current" -> 0,
      "listed" -> false
    ))("Session")
    _ <- tx.lastly(Json.obj(
      "statement" ->
        """MATCH (s:Session {uri: {sessionid}}), (u:User {uri: {userid}})
           CREATE (u)-[:MENTORS]->(s) """,
      "parameters" -> Json.obj(
        "sessionid" -> uri,
        "userid" -> mentor
      )))
  } yield uri

  def sessionMentors (mentor: String): Future[JsArray] = for {
    result <- query(Json.obj(
      "statement" -> """MATCH (u:User {uri: {userid}})-[:MENTORS]->(s:Session)
                        RETURN s""",
      "parameters" -> Json.obj(
        "userid" -> mentor
      )))
  } yield JsArray(result("s"))

  def sessionIsOwner (user: String, session: String): Future[Boolean] = for {
    result <- query(Json.obj(
      "statement" -> """MATCH (s:Session {uri: {sessionid}})<-[:MENTORS]-(u:User {uri: {userid}})
                        RETURN u.uri""",
      "parameters" -> Json.obj(
        "sessionid" -> session,
        "userid" -> user
      )
    ))
  } yield result("u.uri").length match {
    case 0 => false
    case _ => true
  }
}
