/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.user._

/** Play Framework controller for everything related to Arckane's app serving and authentication. */
class Application extends Controller {

  /** Serves the app if authenticated, otherwise serves the authentication page.
    * Route: GET /
    * Session variables: name, home
    */
  def index = Action { request =>
    (for {
      name <- request.session.get("name")
      home <- request.session.get("home")
    } yield Ok(views.html.app(Json.obj(
      "name" -> name,
      "home" -> home
    ).toString))).getOrElse {
      Ok(views.html.index())
    }
  }

  /** Tries to authenticate a user and responds with a json object.
    * Route: POST /signin
    * Form variables: email, password
    */
  def signin = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val email = form("email").head
      val password = form("password").head
      UserTag.authenticate(email, password).map {
        case Some(user) => Ok(Json.obj("success" -> true, "user" -> user))
          .withSession(
            "email" -> (user \ "email").as[String],
            "name" -> (user \ "name").as[String],
            "home" -> (user \ "url").as[String])
        case None => Ok(Json.obj("success" -> false))
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  /** Deletes the current session to sign out a user.
    * Route: GET /signout
    */
  def signout = Action {
    Ok(views.html.index()).withNewSession
  }
}
