package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._
import decision.user._

class Application extends Controller {

  def index = Action {
    Ok(views.html.index())
  }

  def signin = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val email = form("email").head
      val password = form("password").head
      UserTag.authenticate(email, password).map {
        case Some(user) => Ok(Json.obj("success" -> true, "user" -> user))
        case None => Ok(Json.obj("success" -> false))
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

}
