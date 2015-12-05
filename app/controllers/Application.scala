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
    } yield Ok(views.html.index(Json.obj(
      "name" -> name,
      "home" -> home
    ).toString))).getOrElse {
      Ok(views.html.index(Json.obj(
        "guest" -> true
      ).toString))
    }
  }

  def signin = Action { request =>
    (for {
      home <- request.session.get("home")
    } yield Redirect("/")).getOrElse {
      Ok(views.html.signin(Json.obj(
        "guest" -> true
      ).toString))
    }
  }

  def signup = Action { request =>
    (for {
      home <- request.session.get("home")
    } yield Redirect("/")).getOrElse {
      Ok(views.html.signup(Json.obj(
        "guest" -> true
      ).toString))
    }
  }
}
