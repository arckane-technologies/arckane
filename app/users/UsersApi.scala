/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.users

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.Tag
import arckane.db.transaction._
import arckane.users.user._

/** Play Framework controller for the users service. */
class UsersApi extends Controller {

  /** Set a property of a user in the database.
    * Route: POST /api/users/props
    * Query string variables: user, prop, value
    */
  def setProp = Action.async { request =>
    (for {
      user <- request.queryString.get("user")
      prop <- request.queryString.get("prop")
      value <- request.queryString.get("value")
      response <- Some(Tag.set(user.head, Json.obj(
        prop.head -> value.head
      )))
    } yield response.map { props =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected user, prop and value in the query string."))
    }
  }

  /** Get all the properties of a user in the database.
    * Route: GET /api/users/props
    * Query string variables: user
    */
  def getProps = Action.async { request =>
    (for {
      user <- request.queryString.get("user")
      response <- Some(Tag.get(user.head))
    } yield response.map { props =>
      props match {
        case Some(props) => Ok(props)
        case None => NotFound
      }
    }).getOrElse {
      Future(BadRequest("Expected a user in the query string."))
    }
  }

  /** Tries to signup a user and responds with a json object.
    * Route: POST /api/users/signup
    * Form variables: invitation, firstname, lastname, email, password, birthday
    */
  def signup = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val invitation = ""//form("invitation").head
      val firstname = form("firstname").head
      val lastname = form("lastname").head
      val email = form("email").head
      val password = form("password").head
      for {
        tx <- openTransaction
        uri <- Tag.create(tx, Json.obj(
          "firstname" -> firstname,
          "lastname" -> lastname,
          "email" -> email,
          "password" -> password,
          "rating" -> 0
        ))("User")
        _ <- if (invitation != "") tx.lastly(Json.obj(
          "statement" ->
            ( "MATCH (host:User)-[r:INVITES]->(i:InvitationPending {uuid: {invitation}}),(u:User {uri: {urimatcher}})"
            + "DELETE r,i CREATE (host)-[:INVITES]->(u)"),
          "parameters" -> Json.obj(
            "invitation" -> invitation,
            "urimatcher" -> uri)))
          else tx.finish
      } yield Ok(Json.obj("success" -> true))
          .withSession(
            "email" -> email,
            "name" -> firstname,
            "home" -> uri)
    }.getOrElse {
      Future(BadRequest("Expected a uri encoded form."))
    }
  }

  /** Tries to authenticate a user and responds with a json object.
    * Route: POST /api/users/signin
    * Form variables: email, password
    */
  def signin = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val email = form("email").head
      val password = form("password").head
      authenticate(email, password).map {
        case Some(user) =>
          Ok(Json.obj(
            "success" -> true,
            "user" -> user
          )).withSession(
            "email" -> (user \ "email").as[String],
            "name" -> (user \ "name").as[String],
            "home" -> (user \ "uri").as[String]
          )
        case None =>
          Ok(Json.obj(
            "success" -> false
          ))
      }
    }.getOrElse {
      Future(BadRequest("Expected a uri encoded form."))
    }
  }

  /** Deletes the current session to sign out a user.
    * Route: GET /api/users/signout
    */
  def signout = Action {
    Redirect("/").withNewSession
  }
}
