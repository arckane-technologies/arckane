package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._
import decision.user._
import decision.skill._
import decision.skillbook._

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

  def user (id: String) = Action.async { request =>
    UserTag.getWith[UserInfo]("/user/"+id).map {
      case Some(arcklet) => Ok(views.html.user("/user/"+id, arcklet.props.name))
      case None => NotFound("ERROR 404: User "+id+" not found.")
    }
  }

  def skillbook (id: String) = Action.async { request =>
    SkillbookTag.getPageData("/skillbook/"+id).map {
      case Some(pageData) => Ok(views.html.book(pageData("name"), pageData("url"), pageData("description"), pageData("data")))
      case None => NotFound("ERROR 404: Skillbook "+id+" not found.")
    }
  }

  def skill (book: String, skill: String) = Action.async { request =>
    SkillTag.getPageData("/skill/"+skill, "/skillbook/"+book).map {
      case Some(pageData) => Ok(views.html.skill(pageData("name"), pageData("skillbookName"), pageData("url"), pageData("skillbookUrl"), pageData("description"), pageData("data")))
      case None => NotFound("ERROR 404: Skill "+skill+" not found.")
    }
  }
}
