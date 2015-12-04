/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package users

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of User. */
package object user {

  /** User type. */
  trait User

  /** Data type for Neo4j :User tag. Specializes the more general type [[database.persistence.Tag]]. */
  object UserTag extends Tag[User]("User")

  /** Type class for the [[UserTag]] data type. */
  implicit class UserTagOps (tag: Tag[User]) {

    /** Authenticates the user, returns a json with basic info. */
    def authenticate (email: String, password: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {email: {emailmatch}, password: {passmatch}}) RETURN n.url, n.name"),
        "parameters" -> Json.obj(
          "emailmatch" -> email,
          "passmatch" -> password
        )))
    } yield if (result(0)("n.url").length == 0)
        None
      else {
        Some(Json.obj(
          "url" -> result(0)("n.url")(0),
          "email" -> email,
          "name" -> result(0)("n.name")(0)
        ))
      }

    /** Sets the lat and lon attributes of a user in the database.
      *
      * @param user to be updated.
      * @param lat latitude to be set.
      * @param lon longitude to be set.
      */
    def setLocation (user: String, lat: Double, lon: Double): Future[Unit] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (u:"+tag.str+" {url: {user}}) SET u.lat = {lat}, u.lon = {lon}"),
        "parameters" -> Json.obj(
          "user" -> user,
          "lat" -> lat,
          "lon" -> lon
        )
      ))
    } yield Unit

    /** Retrives the latitude and longitude of a user from the database.
      *
      * @param user to be retrive the location from.
      * @return a json object with the location.
      */
    def getLocation (user: String): Future[JsValue] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (u:"+tag.str+" {url: {user}}) RETURN u.lat, u.lon"),
        "parameters" -> Json.obj(
          "user" -> user
        )
      ))
    } yield if (result(0)("u.lat").length == 1) Json.obj(
      "lat" -> result(0)("u.lat")(0),
      "lon" -> result(0)("u.lon")(0)
    ) else Json.obj("error" -> "no such user")
  }
}