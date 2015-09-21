package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import database.skill._

/** Play Framework controller for everything related to Arckane's "Skills". */
class SkillApi extends Controller {

  /** Retrieves from the database all the data needed to display the skill page.
    * Route: /api/skill/data
    * Query string variables: id
    */
  def page = Action.async { request =>
    request.queryString.get("id").map { skillId =>
      SkillTag.getPageData("/skill/"+skillId.head).map {
        case Some(pageData) => Ok(pageData)
        case None => NotFound("ERROR 404: Skill "+skillId.head+" not found.")
      }
    }.getOrElse {
      Future(BadRequest("Expected 'id' query string."))
    }
  }

  /** Creates a new skill proposed by a user.
    * Route: /api/skill/propose
    * Session variables: home
    * Form variables: name, description
    */
  def propose = Action.async { request =>
    (for {
      home <- request.session.get("home")
      form <- request.body.asFormUrlEncoded
      response <- Some(SkillTag.propose(form("name").head, form("description").head, home))
    } yield response.map { json =>
      Ok(json)
    }).getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }

  /** Searches through the database for skill with a similar name to the provided
    * query string, returns an array of possible skills, with its name and url.
    * Route: /api/skill/search
    * Query string variables: search
    */
  def search = Action.async { request =>
    request.queryString.get("search").map { search =>
      SkillTag.search(search.head).map { array =>
        Ok(array)
      }
    }.getOrElse {
      Future(BadRequest("Expected 'search' query string."))
    }
  }
}
