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
import database.resource._

/** Play Framework controller for everything related to Arckane's "Resources". */
class ResourceApi extends Controller {

  /** Creates either an extenrnal resource or a relationship between skills.  
    * Route: /api/resource/propose
    * Session variables: home
    * Form variables:
    *   1) type, forSkill, relatedSkill
    *   2) type, forSkill, name, url, description
    */
  def propose = Action.async { request =>
    (for {
      user <- request.session.get("home")
      form <- request.body.asFormUrlEncoded
      response <- if (form("type").head == "skill") {
        Some(ResourceTag.proposeSkillRelationship(
          form("forSkill").head,
          form("relatedSkill").head,
          user))
      } else {
        Some(ResourceTag.propose(
          form("type").head,
          form("forSkill").head,
          form("name").head,
          form("url").head,
          form("description").head,
          user))
      }
    } yield response.map { json =>
      Ok(json)
    }).getOrElse {
      Future(BadRequest("Expected a url encoded form."))
    }
  }
}
