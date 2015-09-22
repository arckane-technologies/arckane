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

/** Play Framework controller for some Arckane's misc features. */
class FrontendApi extends Controller {

  /** Checks if a string of an attribute of the nodes with a tag (or tags) is not taken.
    * Route: GET /api/availability
    * Query string variables: tags, attribute, value
    */
  def searchAvailability = Action.async { request =>
    (for {
      tags <- request.queryString.get("tags")
      attribute <- request.queryString.get("attribute")
      value <- request.queryString.get("value")
      response <- Some(for {
        result <- query(Json.obj(
          "statement" -> ("MATCH (n) WHERE n"+tags.head+" AND n."+attribute.head+" = {value} RETURN count(n)"),
          "parameters" -> Json.obj(
            "value" -> value.head.trim.clean
        )))
      } yield if (result(0)("count(n)")(0).as[Int] > 0) {
        Json.obj("invalid" -> true)
      } else {
        Json.obj("invalid" -> false)
      })
    } yield response.map { json =>
      Ok(json)
    }).getOrElse {
      Future(BadRequest("Expected 'name', 'attribute' and 'tags' query strings."))
    }
  }

  /** Retrieves all the skills, skillbooks and resources in the database that their
    * name may be similar to the provided query string.
    * Route: GET /api/search
    * Query string variables: search
    */
  def search = Action.async { request =>
    request.queryString.get("search").map { search =>
      val queryString = search.head.clean.escapeParenthesis
      for {
        result <- query(Json.arr(Json.obj(
          // SEARCH: Skills
          "statement" -> "MATCH (n:Skill) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description",
          "parameters" -> Json.obj(
            "regex" -> ("(?i).*"+queryString+".*")
          )), Json.obj(
          // SEARCH: Skillbooks
          "statement" -> "MATCH (n:Skillbook) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description",
          "parameters" -> Json.obj(
            "regex" -> ("(?i).*"+queryString+".*")
          )), Json.obj(
          // SEARCH: Resources
          "statement" -> "MATCH (n:Resource) WHERE n.name =~ { regex } RETURN n.name, n.url, n.description, n.resourceType, n.resourceUrl",
          "parameters" -> Json.obj(
            "regex" -> ("(?i).*"+queryString+".*")
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
}
