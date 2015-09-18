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

  def skillInfo = Action.async { request =>
    request.queryString.get("id").map { skillId =>
      SkillTag.getPageData("/skill/"+skillId.head).map {
        case Some(pageData) => Ok(pageData)
        case None => NotFound("ERROR 404: Skill "+skillId.head+" not found.")
      }
    }.getOrElse {
      Future(BadRequest("Expected 'id' query string."))
    }
  }

  def userInfo = Action.async { request =>
    Future(Ok("Hola :P"));
    //UserTag.getWith[UserInfo]("/user/"+id).map {
    //  case Some(arcklet) => Ok(views.html.user("/user/"+id, arcklet.props.name))
    //  case None => NotFound("ERROR 404: User "+id+" not found.")
    //}
  }

  def skillbookInfo = Action.async { request =>
    (for {
      a <- request.queryString.get("id")
      b <- request.session.get("home")
    } yield (a.head, b)).map { case (skillbookId, user) =>
      SkillbookTag.getPageData("/skillbook/"+skillbookId, user).map {
        case Some(pageData) => Ok(pageData)
        case None => NotFound("ERROR 404: Skillbook "+skillbookId+" not found.")
      }
    }.getOrElse {
      Future(BadRequest("Expected 'id' query string."))
    }
  }

  def skillbookSubsection = Action.async { request =>
    (for {
      a <- request.queryString.get("source")
      b <- request.queryString.get("skillbook")
      c <- request.queryString.get("depth")
    } yield (a.head, b.head, c.head.toInt)).map { case (source, skillbook, depth) =>
      SkillbookTag.getSubsection(source, skillbook, depth).map { array =>
        Ok(array)
      }
    }.getOrElse {
      Future(BadRequest("Expected 'source', 'skillbook' and 'depth' query strings."))
    }
  }

  def changeSkillbookName = Action.async { request =>
    (for {
      a <- request.session.get("home")
      b <- request.queryString.get("id")
      c <- request.queryString.get("name")
    } yield (a, b.head, c.head)).map { case (user, skillbookId, name) =>
      SkillbookTag.changeName("/skillbook/"+skillbookId, name, user).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  def addSkillToSkillbook = Action.async { request =>
    (for {
      a <- request.queryString.get("source")
      b <- request.queryString.get("target")
      c <- request.queryString.get("skillbook")
      d <- request.queryString.get("depth")
    } yield (a.head, b.head, c.head, d.head)).map { case (source, target, skillbook, depth) =>
      SkillbookTag.addSkill(source, target, skillbook, depth.toInt).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected 'source', 'skillbook', 'depth' and 'target' query strings."))
    }
  }

  def changeSkillInSkillbook = Action.async { request =>
    (for {
      a <- request.queryString.get("skill")
      b <- request.queryString.get("oldSkill")
      c <- request.queryString.get("skillbook")
      d <- request.queryString.get("depth")
    } yield (a.head, b.head, c.head, d.head.toInt)).map { case (skill, oldSkill, skillbook, depth) =>
      SkillbookTag.changeSkill(skill, oldSkill, skillbook, depth).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected 'skill', 'oldSkill', 'skillbook', and 'depth' query strings."))
    }
  }

  def deleteSkillInSkillbook = Action.async { request =>
    (for {
      a <- request.queryString.get("skill")
      b <- request.queryString.get("skillbook")
      c <- request.queryString.get("depth")
    } yield (a.head, b.head, c.head.toInt)).map { case (skill, skillbook, depth) =>
      SkillbookTag.deleteSkill(skill, skillbook, depth).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected 'skill', 'skillbook', and 'depth' query strings."))
    }
  }

  def deleteSkillbook = Action.async { request =>
    request.queryString.get("skillbook").map { skillbook =>
      SkillbookTag.deleteSkillbook(skillbook.head).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected 'skillbook' query strings."))
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
      Future(BadRequest("Expected 'search' query string."))
    }
  }

  def searchSkill = Action.async { request =>
    request.queryString.get("search").map { search =>
      for {
        result <- query(Json.obj(
          // SEARCH: Skills
          "statement" -> "MATCH (n:Skill) WHERE n.name =~ { regex } RETURN n.name, n.url",
          "parameters" -> Json.obj(
            "regex" -> ("(?i).*"+search.head+".*")
        )))
      } yield if (result(0)("n.url").length > 0) {
        val data = (result(0)("n.url") zip result(0)("n.name"))
        Ok(data.foldLeft(Json.arr()) { (array, data) =>
          array :+ Json.obj("url" -> data._1, "name" -> data._2)
        })
      } else {
        Ok(Json.arr())
      }
    }.getOrElse {
      Future(BadRequest("Expected 'search' query string."))
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
      Future(BadRequest("Expected 'name', 'attribute' and 'tags' query strings."))
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
