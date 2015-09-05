package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.user._

class Application extends Controller {

  def index = Action { request =>
    (for {
      name <- request.session.get("name")
      home <- request.session.get("home")
    } yield Ok(views.html.app(Json.obj(
      "name" -> name,
      "home" -> home,
      "skillbooks" -> Json.arr()
    ).toString))).getOrElse {
      Ok(views.html.index())
    }
  }

  def signin = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val email = form("email").head
      val password = form("password").head
      UserTag.authenticate(email, password).map {
        case Some(user) => Ok(Json.obj("success" -> true, "user" -> user))
          .withSession(
            "connected" -> (user \ "email").as[String],
            "name" -> (user \ "name").as[String],
            "home" -> (user \ "url").as[String])
        case None => Ok(Json.obj("success" -> false))
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  def signout = Action {
    Ok(views.html.index()).withNewSession
  }
}
