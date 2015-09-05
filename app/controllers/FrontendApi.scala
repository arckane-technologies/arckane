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
import decision.resource._

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

  def searchSkill = Action.async { request =>
    request.queryString.get("search").map { search =>
      for {
        result <- query(Json.obj(
          // SEARCH: Skills
          "statement" -> "MATCH (n:Skill) WHERE n.name =~ { regex } RETURN n.name",
          "parameters" -> Json.obj(
            "regex" -> ("(?i).*"+search.head+".*")
        )))
      } yield if (result.length > 0) {
        Ok(Json.toJson(result(0)("n.name")))
      } else {
        Ok(Json.arr())
      }
    }.getOrElse {
      Future(BadRequest("Expected search query string."))
    }
  }

  def proposeSkill = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      SkillTag.create(Json.obj(
        "name" -> form("name").head,
        "description" -> form("description").head,
        "resourceType" -> "skill"
      )).map { arcklet =>
        Ok(Json.obj("url" -> arcklet.url))
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  def proposeResource = Action.async { request =>
    request.body.asFormUrlEncoded.map { form =>
      val rtype = form("type").head
      val forSkill = form("forSkill").head.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "")
      if (rtype == "skill") {
        val relatedSkill = form("relatedSkill").head.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "")
        for {
          result <- query(Json.obj(
            "statement" -> ("MATCH (a:Skill {name: {source}}),(b:Skill {name: {target}}) MERGE (a)-[:RELATED]->(b) RETURN a.url"),
            "parameters" -> Json.obj(
              "source" -> forSkill,
              "target" -> relatedSkill
            )))
        } yield if (result(0)("a.url").length == 1) {
          Ok(Json.obj("url" -> result(0)("a.url")(0)))
        } else {
          InternalServerError("Something went wrong with the query.")
        }
      } else {
        for {
          arcklet <- ResourceTag.create(Json.obj(
            "name" -> form("name").head,
            "resourceType" -> rtype,
            "resourceUrl" -> form("url").head,
            "description" -> form("description").head
          ))
          result <- query(Json.obj(
            "statement" -> ("MATCH (a:Skill {name: {source}}),(b:Resource {url: {target}}) MERGE (a)-[:HAS_RESOURCE]->(b) RETURN a.url"),
            "parameters" -> Json.obj(
              "source" -> forSkill,
              "target" -> arcklet.url
            )))
        } yield if (result(0)("a.url").length == 1) {
          Ok(Json.obj("url" -> result(0)("a.url")(0)))
        } else {
          InternalServerError("Something went wrong with the query.")
        }
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }
}
