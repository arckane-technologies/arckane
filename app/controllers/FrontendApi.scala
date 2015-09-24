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
    * Sessions variables: home
    * Query string variables: search
    */
  def search = Action.async { request =>
    (for {
      user <- request.session.get("home")
      search <- request.queryString.get("search")
      response <- Some({
        val queryString = search.head.clean.escapeParenthesis
        for {
          result <- query(Json.arr(Json.obj(
            // SEARCH: Skills
            "statement" ->
              ( "MATCH (n:Skill) WHERE n.name =~ { regex } "
              + "RETURN n.name, n.url, n.description"),
            "parameters" -> Json.obj(
              "regex" -> ("(?i).*"+queryString+".*")
            )), Json.obj(
            // SEARCH: Skillbooks
            "statement" ->
              ( "MATCH (n:Skillbook) WHERE n.name =~ { regex } "
              + "RETURN n.name, n.url, n.description"),
            "parameters" -> Json.obj(
              "regex" -> ("(?i).*"+queryString+".*")
            )), Json.obj(
            // SEARCH: Resources
            "statement" ->
              ( "MATCH (n:Resource) WHERE n.name =~ { regex } "
              + "OPTIONAL MATCH (:User {url: {user}})-[r:INFUSES]->(n)"
              + "RETURN n.name, n.url, n.description, n.resourceType, n.resourceUrl, n.infusionValue, r.infusionValue"),
            "parameters" -> Json.obj(
              "regex" -> ("(?i).*"+queryString+".*"),
              "user" -> user
            ))))
        } yield if (result.length > 0) {
          // RESULT: Skills
          Json.toJson(result(0)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(0)("n.url")(index),
            "resourceType" -> "skill",
            "infusionSourceSkill" -> "",
            "infusionTarget" -> "",
            "infusionValue" -> 0,
            "infusionUserVote" -> 0,
            "description" -> result(0)("n.description")(index))
          }).as[JsArray] ++
          // RESULT: Skillbooks
          Json.toJson(result(1)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(1)("n.url")(index),
            "resourceType" -> "skillbook",
            "infusionSourceSkill" -> "",
            "infusionTarget" -> "",
            "infusionValue" -> 0,
            "infusionUserVote" -> 0,
            "description" -> result(1)("n.description")(index))
          }).as[JsArray] ++
          // RESULT: Resources
          Json.toJson(result(2)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(2)("n.resourceUrl")(index),
            "resourceType" -> result(2)("n.resourceType")(index),
            "infusionSourceSkill" -> "is-resource",
            "infusionTarget" -> result(2)("n.url")(index),
            "infusionValue" -> result(2)("n.infusionValue")(index),
            "infusionUserVote" -> {
              if (result(2)("r.infusionValue")(index) == JsNull) JsNumber(0)
              else result(2)("r.infusionValue")(index)
            },
            "description" -> result(2)("n.description")(index))
          }).as[JsArray]
        } else {
          Json.arr()
        }
      })
    } yield response.map { array =>
      Ok(array)
    }).getOrElse {
      Future(BadRequest("Expected 'search' query string."))
    }
  }

  /** Sets the requested state for an infusion, a user up-votes or down-votes the
    * relationship between two skills or a resource itself.
    * Route: PUT /api/infuse
    * Sessions variables: home
    * Query string variables: target, source, state
    */
  def infuse = Action.async { request =>
    (for {
      user <- request.session.get("home")
      target <- request.queryString.get("target")
      source <- request.queryString.get("source")
      state <- request.queryString.get("state")
      response <- Some({
        if (source.head == "is-resource")
          infuseResource(target.head, state.head.toInt, user)
        else
          infuseSkillRelationship(target.head, source.head, state.head.toInt, user)
      })
    } yield response.map { json =>
      Ok(json)
    }).getOrElse {
      Future(BadRequest("Expected 'target', 'source' and 'state' query string."))
    }
  }

  /** Sets the requested state for an infusion, a user up-votes or down-votes the
    * relationship between two skills or a resource itself. Last state is checked
    * (oldState) so that a user can't duplicate voting.
    *
    * @param target url-id of the resource to be infused.
    * @param state or vote the user is changing to.
    * @param user url-id which is infusing.
    * @return a json object with the state the user tried to change to. {value: number}
    */
  private def infuseResource (target: String, state: Int, user: String): Future[JsObject] = for {
    tx <- openTransaction
    oldState <- tx.execute(Json.obj(
      "statement" ->
        ( "OPTIONAL MATCH (:User {url: {user}})-[rel:INFUSES]->(:Resource {url: {target}}) "
        + "RETURN rel.infusionValue"),
      "parameters" -> Json.obj(
        "user" -> user,
        "target" -> target
      )
    ))
    result <- if (oldState(0)("rel.infusionValue")(0) == JsNumber(state)) {
      tx.finish
      Future(List(): TxResult)
    } else {
      tx.lastly(Json.arr(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {user}}),(r:Resource {url: {target}}) "
          + "MERGE (u)-[rel:INFUSES]->(r) "
          + "SET r.infusionValue = r.infusionValue - {oldState} + {state}, rel.infusionValue = {state} "),
        "parameters" -> Json.obj(
          "user" -> user,
          "target" -> target,
          "oldState" -> {
            if (oldState(0)("rel.infusionValue")(0) == JsNull) JsNumber(0)
            else oldState(0)("rel.infusionValue")(0)
          },
          "state" -> state
        )
      )))
    }
  } yield Json.obj("value" -> state)

  /** Sets the requested state for an infusion, a user up-votes or down-votes the
    * relationship between two skills or a resource itself. Last state is checked
    * (oldState) so that a user can't duplicate voting.
    *
    * @param target url-id of the skill that is related from the source.
    * @param source url-id of skill containing the target.
    * @param state or vote the user is changing to.
    * @param user url-id which is infusing.
    * @return a json object with the state the user tried to change to. {value: number}
    */
  private def infuseSkillRelationship (target: String, source: String, state: Int, user: String): Future[JsObject] = for {
    tx <- openTransaction
    oldState <- tx.execute(Json.obj(
      "statement" ->
        ( "OPTIONAL MATCH (:User {url: {user}})-[rel:INFUSES {source: {source}}]->(:Skill {url: {target}}) "
        + "RETURN rel.infusionValue"),
      "parameters" -> Json.obj(
        "user" -> user,
        "target" -> target,
        "source" -> source
      )
    ))
    result <- if (oldState(0)("rel.infusionValue")(0) == JsNumber(state)) {
      tx.finish
      Future(List(): TxResult)
    } else {
      tx.lastly(Json.arr(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {user}}),(s:Skill {url: {source}})-[r:RELATED]->(t:Skill {url: {target}})"
          + "MERGE (u)-[rel:INFUSES {source: {source}}]->(t) "
          + "SET r.infusionValue = r.infusionValue - {oldState} + {state}, rel.infusionValue = {state} "),
        "parameters" -> Json.obj(
          "user" -> user,
          "target" -> target,
          "source" -> source,
          "oldState" -> {
            if (oldState(0)("rel.infusionValue")(0) == JsNull) JsNumber(0)
            else oldState(0)("rel.infusionValue")(0)
          },
          "state" -> state
        )
      )))
    }
  } yield Json.obj("value" -> state)
}
