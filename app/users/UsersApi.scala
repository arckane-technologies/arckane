/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package users

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import users.user._

/** Play Framework controller for the users service. */
class UsersApi extends Controller {

  /** Tries to signup a user and responds with a json object.
    * Route: POST /api/users/signup
    * Form variables: invitation, firstname, lastname, email, password, birthday
    */
  def signup = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val invitation = form("invitation").head
      val firstname = form("firstname").head
      val lastname = form("lastname").head
      val email = form("email").head
      val password = form("password").head
      for {
        tx <- openTransaction
        user <- UserTag.create(tx, Json.obj(
          "firstname" -> firstname,
          "lastname" -> lastname,
          "email" -> email,
          "password" -> password
        ))
        _ <- if (invitation != "") tx.lastly(Json.obj(
          "statement" ->
            ( "MATCH (host:User)-[r:INVITES]->(i:InvitationPending {uuid: {invitation}}),(u:User {url: {user}})"
            + "DELETE r,i CREATE (host)-[:INVITES]->(u)"),
          "parameters" -> Json.obj(
            "invitation" -> invitation,
            "user" -> user.url)))
          else tx.finish
      } yield Ok(Json.obj("success" -> true))
          .withSession(
            "email" -> email,
            "name" -> firstname,
            "home" -> user.url)
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  /** Tries to authenticate a user and responds with a json object.
    * Route: POST /api/users/signin
    * Form variables: email, password
    */
  def signin = Action.async { request =>
    //request.body.asFormUrlEncoded.map { form =>
    //  val email = form("email").head
    //  val password = form("password").head
    //  UserTag.authenticate(email, password).map {
    //    case Some(user) => Ok(Json.obj("success" -> true, "user" -> user))
    //      .withSession(
    //        "email" -> (user \ "email").as[String],
    //        "name" -> (user \ "name").as[String],
    //        "home" -> (user \ "url").as[String])
    //    case None => Ok(Json.obj("success" -> false))
    //  }
    //}.getOrElse {
    //  Future(BadRequest("Expected a url encoded form."))
    //}
    Future(Ok)
  }

  /** Deletes the current session to sign out a user.
    * Route: GET /api/users/signout
    */
  def signout = Action {
    Ok(views.html.index()).withNewSession
  }
}
