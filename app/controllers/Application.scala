package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._
import decision.user._

class Application extends Controller {

  def index = Action { request =>
    val userId = """/user/(.+)""".r
    request.session.get("home").map { url =>
      url match {
        case userId(home) => Redirect(routes.Application.user(home))
        case _ => InternalServerError("Invalid home value in session.")
      }
    }.getOrElse {
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

  def user (id: String) = Action { request =>
    // CHANGE ME: Load from db the user profile
    request.session.get("name").map { name =>
      Ok(views.html.user("/user/"+id, name))
    }.getOrElse {
      InternalServerError("Invalid name value in session.")
    }
  }
}
