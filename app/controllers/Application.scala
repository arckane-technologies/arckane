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
    Future(Ok("Hola :p"))
  }
  /*
  """{"related": [
    {"name": "Variable", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Some description..."},
    {"name": "Evaluation", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "While Loops", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Lorem Ipsum is simply dummy text of the printing and typesetting industry. Lorem Ipsum has been the industrys standard dummy text ever since the 1500s, when an unknown printer took a galley of type and scrambled it to make a type specimen book. It has survived not only five centuries, but also the leap into electronic typesetting, remaining essentially unchanged. It was popularised in the 1960s with the release of Letraset sheets containing Lorem Ipsum passages, and more recently with desktop publishing software like Aldus PageMaker including versions of Lorem Ipsum. "},
    {"name": "Functional Programming", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "In computer science, functional programming is a programming paradigm—a style of building the structure and elements of computer programs—that treats computation as the evaluation of mathematical functions and avoids changing-state and mutable data."},
    {"name": "Evaluation", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "While Loops", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "Variable", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Some description..."},
    {"name": "Evaluation", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "While Loops", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "Variable", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Some description..."},
    {"name": "Evaluation", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "While Loops", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "Variable", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Some description..."},
    {"name": "Evaluation", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."},
    {"name": "While Loops", "infusionTarget": "/skill/123", "url": "/book/123/skill/123", "content": "Something..."}
  ], "resources": [
    {"name": "Some Book Chapter", "resourceType": "book", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "Cool Video", "resourceType": "video", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "May be Legal PDF", "resourceType": "pdf", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "n-person Games Equilibrium", "resourceType": "paper", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "Standford R01-1", "resourceType": "lecture", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "Google Edu", "resourceType": "slides", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "W3School: HTML", "resourceType": "article", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "Amazing Infographic", "resourceType": "image", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."},
    {"name": "Scala at the Sea", "resourceType": "event", "infusionTarget": "/resource/0", "url": "/skill/123", "content": "Something..."}
  ]}"""
  */
}
