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

class FrontendApi extends Controller {

  def skillInfo () = Action.async { request =>
    request.queryString.get("id").map { skillId =>
      SkillTag.getPageData("/skill/"+skillId.head).map {
        case Some(pageData) => Ok(pageData)
        case None => NotFound("ERROR 404: Skill "+skillId+" not found.")
      }
    }.getOrElse {
      Future(BadRequest("Expected id query string."))
    }
  }

  def userInfo (id: String) = Action.async { request =>
    Future(Ok("Hola :P"));
    //UserTag.getWith[UserInfo]("/user/"+id).map {
    //  case Some(arcklet) => Ok(views.html.user("/user/"+id, arcklet.props.name))
    //  case None => NotFound("ERROR 404: User "+id+" not found.")
    //}
  }

  def skillbookInfo (id: String) = Action.async { request =>
    Future(Ok("Hola :P"));
    //SkillbookTag.getPageData("/skillbook/"+id).map {
    //  case Some(pageData) => Ok(views.html.book(pageData("name"), pageData("url"), pageData("description"), pageData("data")))
    //  case None => NotFound("ERROR 404: Skillbook "+id+" not found.")
    //}
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
            "url" -> result(0)("n.url")(index),
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
          // RESULT: Resources
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
