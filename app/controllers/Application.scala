package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.user._
import decision.skill._
import decision.skillbook._

class Application extends Controller {

  def index = Action { request =>
    request.session.get("name").map { name =>
      Redirect(routes.Application.skillbook("great"))
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

  def search = Action.async { request =>
    request.queryString.get("search").map { search => for {
      result <- query(Json.arr(Json.obj(
        // SEARCH: Skills
        "statement" -> "MATCH (n:Skill) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description",
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+search.head+".*")
        )), Json.obj(
        // SEARCH: Skillbooks
        "statement" -> "MATCH (n:Skillbook) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description",
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+search.head+".*")
        )), Json.obj(
        // SEARCH: Resources
        "statement" -> "MATCH (n:Resource) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description, n.resourceType, n.resourceUrl",
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+search.head+".*")
        ))))
    } yield if (result.length > 0) {
        Ok(
          // RESULT: Skills
          Json.toJson(result(0)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> ("/skillbook/great" + result(0)("n.url")(index).as[String]),
            "resourceType" -> "skill",
            "infusionTarget" -> result(0)("n.url")(index),
            "description" -> result(0)("n.description")(index))
          }).as[JsArray] ++
          // RESULT: Skillbooks
          Json.toJson(result(1)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(1)("n.url")(index),
            "resourceType" -> "skillbook",
            "infusionTarget" -> result(1)("n.url")(index),
            "description" -> result(1)("n.description")(index))
          }).as[JsArray] ++
          // RESULT: Skills
          Json.toJson(result(2)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(2)("n.resourceUrl")(index),
            "resourceType" -> result(2)("n.resourceType")(index),
            "infusionTarget" -> result(2)("n.url")(index),
            "description" -> result(2)("n.description")(index))
          }).as[JsArray]
        )
      } else {
        Ok(Json.arr())
      }
    }.getOrElse {
      Future(BadRequest("Expected search query string."))
    }
  }
}
