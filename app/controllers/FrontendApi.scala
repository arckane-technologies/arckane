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

  def searchAvailability = Action.async { request =>
    (for {
      tags <- request.queryString.get("tags")
      attribute <- request.queryString.get("attribute")
      value <- request.queryString.get("value")
    } yield (tags.head, attribute.head, trim(value.head))).map { case (tags, attribute, value) =>
      for {
        result <- query(Json.obj(
          "statement" -> ("MATCH (n) WHERE n"+tags+" AND n."+attribute+" = {value} RETURN count(n)"),
          "parameters" -> Json.obj(
            "value" -> value
        )))
      } yield if (result(0)("count(n)")(0).as[Int] > 0)
        Ok(Json.obj("invalid" -> true))
      else
        Ok(Json.obj("invalid" -> false))
    }.getOrElse {
      Future(BadRequest("Expected a name, attribute and tags query strings."))
    }
  }

  def proposeSkill = Action.async { request =>
    (for {
      home <- request.session.get("home")
      form <- request.body.asFormUrlEncoded
      response <- Some(for {
        tx <- openTransaction
        skill <- SkillTag.create(tx, Json.obj(
          "name" -> trim(form("name").head),
          "description" -> trim(form("description").head),
          "resourceType" -> "skill"))
        _ <- tx.lastly(Json.obj(
          "statement" -> ("MATCH (u:User {url:{user}}),(s:Skill {url:{skill}}) CREATE (u)-[:PROPOSES]->(s)"),
          "parameters" -> Json.obj(
            "user" -> home,
            "skill" -> skill.url
          )))
      } yield Ok(Json.obj("url" -> skill.url)))
    } yield response).getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  def proposeResource = Action.async { request =>
    (for {
      home <- request.session.get("home")
      form <- request.body.asFormUrlEncoded
      response <- Some({
        val rtype = form("type").head
        val forSkill = trim(form("forSkill").head)
        if (rtype == "skill") {
          val relatedSkill = trim(form("relatedSkill").head)
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
            tx <- openTransaction
            arcklet <- ResourceTag.create(tx, Json.obj(
              "name" -> trim(form("name").head),
              "resourceType" -> rtype,
              "resourceUrl" -> trim(form("url").head),
              "description" -> trim(form("description").head)
            ))
            result <- tx.lastly(Json.obj(
              "statement" -> ("MATCH (a:Skill {name: {source}}),(b:Resource {url: {target}}),(u:User {url: {user}}) MERGE (a)-[:HAS_RESOURCE]->(b)<-[:PROPOSES]-(u) RETURN a.url"),
              "parameters" -> Json.obj(
                "source" -> forSkill,
                "target" -> arcklet.url,
                "user" -> home
              )))
          } yield if (result(0)("a.url").length == 1) {
            Ok(Json.obj("url" -> result(0)("a.url")(0)))
          } else {
            InternalServerError("Something went wrong with the query.")
          }
        }
      })
    } yield response).getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  def proposeSkillbook = Action.async { request =>
    request.session.get("home").map { home =>
      for {
        tx <- openTransaction
        skillbook <- SkillbookTag.create(tx, Json.obj("name" -> "New Skillbook"))
        _ <- tx.lastly(Json.obj(
          "statement" -> ("MATCH (s:Skillbook {url: {skillbook}}),(u:User {url: {user}}) CREATE (u)-[:PIN]->(s)<-[:PROPOSES]-(u)"),
          "parameters" -> Json.obj(
            "skillbook" -> skillbook.url,
            "user" -> home
          )))
      } yield Ok(Json.obj("url" -> skillbook.url))
    }.getOrElse {
      Future(BadRequest("Must be signed in."))
    }
  }

  private def trim (str: String): String =
    str.replaceAll("""^[\s\r\n]+""", "").replaceAll("""[\s\r\n]+$""", "")
}
