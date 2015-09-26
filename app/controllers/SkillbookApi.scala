/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import database.skillbook._

/** Play Framework controller for everything related to Arckane's "Skillbooks". */
class SkillbookApi extends Controller {

  /** Retrieves all the skillbooks that the user has pinned.
    * Route: GET /api/skillbook
    * Session variables: home
    */
  def userSkillbooks = Action.async { request =>
    request.session.get("home").map { user =>
      SkillbookTag.userSkillbooks(user).map { json =>
        Ok(json)
      }
    }.getOrElse {
      Future(Redirect("/"))
    }
  }

  /** Creates a new empty skillbook.
    * Route: POST /api/skillbook/propose
    * Session variables: home
    */
  def propose = Action.async { request =>
    request.session.get("home").map { user =>
      SkillbookTag.propose(user).map { json =>
        Ok(json)
      }
    }.getOrElse {
      Future(BadRequest("Must be signed in."))
    }
  }

  /** Retrives basic data from the database about a skillbook.
    * Route: GET /api/skillbook/data
    * Session variables: home
    * Query string variables: id
    */
  def page = Action.async { request =>
    (for {
      sid <- request.queryString.get("id")
      user <- request.session.get("home")
      response <- Some(SkillbookTag.getPageData("/skillbook/"+sid.head, user))
    } yield response.map {
      case Some(pageData) => Ok(pageData)
      case None => NotFound("ERROR 404: Skillbook "+sid.head+" not found.")
    }).getOrElse {
      Future(BadRequest("Expected 'id' query string."))
    }
  }

  /** Retrives data about a subsection of a skillbook from the databse.
    * Route: GET /api/skillbook/subsection
    * Query string variables: source, skillbook, depth
    */
  def subsection = Action.async { request =>
    (for {
      source <- request.queryString.get("source")
      skillbook <- request.queryString.get("skillbook")
      depth <- request.queryString.get("depth")
      response <- Some(SkillbookTag.getSubsection(source.head, skillbook.head, depth.head.toInt))
    } yield response.map { array =>
      Ok(array)
    }).getOrElse {
      Future(BadRequest("Expected 'source', 'skillbook' and 'depth' query strings."))
    }
  }

  /** Queries the database to change a skillbook name.
    * Route: PUT /api/skillbook/name
    * Session variables: home
    * Query string variables: id, name
    */
  def changeName = Action.async { request =>
    (for {
      user <- request.session.get("home")
      sid <- request.queryString.get("id")
      name <- request.queryString.get("name")
      response <- Some(SkillbookTag.changeName("/skillbook/"+sid.head, name.head, user))
    } yield response.map { Unit =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected 'id', and 'description' query strings."))
    }
  }

  /** Queries the database to change a skillbook description.
    * Route: PUT /api/skillbook/description
    * Session variables: home
    * Query string variables: id, description
    */
  def changeDescription = Action.async { request =>
    (for {
      user <- request.session.get("home")
      sid <- request.queryString.get("id")
      description <- request.queryString.get("description")
      response <- Some(SkillbookTag.changeDescription("/skillbook/"+sid.head, description.head, user))
    } yield response.map { Unit =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected 'id', and 'description' query strings."))
    }
  }

  /** Adds a skill to a subsection of a skillbook.
    * Route: POST /api/skillbook/skill
    * Query string variables: source, target, skillbook, depth
    */
  def addSkill = Action.async { request =>
    (for {
      source <- request.queryString.get("source")
      target <- request.queryString.get("target")
      skillbook <- request.queryString.get("skillbook")
      depth <- request.queryString.get("depth")
      response <- Some(SkillbookTag.addSkill(source.head, target.head, skillbook.head, depth.head.toInt))
    } yield response.map { Unit =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected 'source', 'skillbook', 'depth' and 'target' query strings."))
    }
  }

  /** Changes a skill of a subsection in a skillbook.
    * Route: PUT /api/skillbook/skill
    * Query string variables: skill, oldSkill, skillbook, depth
    */
  def changeSkill = Action.async { request =>
    (for {
      skill <- request.queryString.get("skill")
      oldSkill <- request.queryString.get("oldSkill")
      skillbook <- request.queryString.get("skillbook")
      depth <- request.queryString.get("depth")
      response <- Some(SkillbookTag.changeSkill(skill.head, oldSkill.head, skillbook.head, depth.head.toInt))
    } yield response.map { Unit =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected 'skill', 'oldSkill', 'skillbook', and 'depth' query strings."))
    }
  }

  /** Toggles a relationship between a user and a skillbook known as :PIN.
    * Route: PUT /api/skillbook/pin
    * Session variables: home
    * Query string variables: id
    */
  def pin = Action.async { request =>
    (for {
      user <- request.session.get("home")
      sid <- request.queryString.get("id")
      response <- Some(SkillbookTag.pinToggle(user, "/skillbook/"+sid.head))
    } yield response.map { pinned =>
      Ok(pinned)
    }).getOrElse {
      Future(BadRequest("Expected 'id' query string."))
    }
  }

  /** Removes a skill from a subsection of a skillbook.
    * Route: DELETE /api/skillbook/skill
    * Query string variables: skill, skillbook, depth
    */
  def deleteSkill = Action.async { request =>
    (for {
      skill <- request.queryString.get("skill")
      skillbook <- request.queryString.get("skillbook")
      depth <- request.queryString.get("depth")
      response <- Some(SkillbookTag.deleteSkill(skill.head, skillbook.head, depth.head.toInt))
    } yield response.map { Unit =>
      Ok
    }).getOrElse {
      Future(BadRequest("Expected 'skill', 'skillbook', and 'depth' query strings."))
    }
  }

  /** Deletes a skillbook from the database.
    * Route: DELETE /api/skillbook
    * Query string variables: skillbook
    */
  def delete = Action.async { request =>
    request.queryString.get("skillbook").map { skillbook =>
      SkillbookTag.delete(skillbook.head).map { Unit =>
        Ok
      }
    }.getOrElse {
      Future(BadRequest("Expected 'skillbook' query strings."))
    }
  }
}
