/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.ws._
import play.api.Play.current

import database.neo4j._
import database.persistence._

/** Play Framework controller for some Arckane's misc features. */
class FrontendApi extends Controller {

  /** sparkpost.com api key needed to send emails. */
  private val sparkpostApikey = Play.current.configuration.getString("sparkpost.apikey").get

  /** Checks if a string of an attribute of the nodes with a tag (or tags) is not taken.
    * Route: GET /api/availability
    * Query string variables: tags, attribute, value, email, cleaning
    */
  def searchAvailability = Action.async { request =>
    (for {
      tags <- request.queryString.get("tags")
      attribute <- request.queryString.get("attribute")
      value <- request.queryString.get("value")
      email <- request.queryString.get("email")
      cleaning <- request.queryString.get("cleaning")
      search <- Some(value.head.trim.clean(cleaning.head))
      response <- Some(for {
        result <- query(Json.obj(
          "statement" -> ("MATCH (n) WHERE n"+tags.head+" AND n."+attribute.head+" =~ {value} RETURN count(n)"),
          "parameters" -> Json.obj(
            "value" -> ("(?i)"+search.escapeParenthesis)
        )))
      } yield if (result(0)("count(n)")(0).as[Int] > 0) {
        Json.obj("invalid" -> true, "value" -> search)
      } else {
        Json.obj("invalid" -> false, "value" -> search)
      })
    } yield response.map { json =>
      Ok(json)
    }).getOrElse {
      Future(BadRequest("Expected 'email', 'cleaning', 'name', 'attribute' and 'tags' query strings."))
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
              + "RETURN n.name, n.url, n.description "
              + "LIMIT 5"),
            "parameters" -> Json.obj(
              "regex" -> ("(?i).*"+queryString+".*")
            )), Json.obj(
            // SEARCH: Skillbooks
            "statement" ->
              ( "MATCH (n:Skillbook) WHERE n.name =~ { regex } "
              + "RETURN n.name, n.url, n.description "
              + "LIMIT 5"),
            "parameters" -> Json.obj(
              "regex" -> ("(?i).*"+queryString+".*")
            )), Json.obj(
            // SEARCH: Resources
            "statement" ->
              ( "MATCH (n:Resource) WHERE n.name =~ { regex } "
              + "OPTIONAL MATCH (:User {url: {user}})-[r:INFUSES]->(n)"
              + "RETURN n.name, n.url, n.description, n.resourceType, n.resourceUrl, n.infusionValue, r.infusionValue "
              + "LIMIT 5"),
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

  /** Sends an invitation to signup to Arckane.
    * Route: POST /api/invite
    * Sessions variables: home
    * Query string variables: email
    */
  def inviteFriend = Action.async { request =>
    (for {
      user <- request.session.get("home")
      form <- request.body.asFormUrlEncoded
      response <- Some(invite(user, form("email").head))
    } yield response.map { info =>
      Ok(info)
    }).getOrElse {
      Future(BadRequest("Expected 'email' query string."))
    }
  }

  /** Sends and saves in the database a friend invite.
    *
    * @param user url-id of the user that is inviting.
    * @param email of the invited person.
    * @return JsObject with information about the success of the invitation.
    */
  def invite (user: String, email: String): Future[JsObject] = for {
    tx <- openTransaction
    isAlreadyUser <- checkIfUserExists(tx, email)
    invUUID <- invitationUUID(tx, user, email)
    result <- if (!isAlreadyUser && invUUID == "") {
      tx.lastly(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {user}}) "
          + "CREATE (u)-[:INVITES]->(i:InvitationPending {uuid: {uuid}, email: {email}}) "
          + "RETURN i.uuid"),
        "parameters" -> Json.obj(
          "user" -> user,
          "email" -> email,
          "uuid" -> uuid
        )
      ))
    } else {
      tx.finish
      Future(List(): TxResult)
    }
    info <- if (isAlreadyUser)
      Future(Json.obj("success" -> true, "message" -> "Your friend is already a mage adept in Arckane!"))
    else if (invUUID != "")
      sendInvitationMail(email, invUUID, "Your invitation has been resent!")
    else
      sendInvitationMail(email, result(0)("i.uuid")(0).as[String], "Your invitation has been sent!")
  } yield info

  /** Check if user with email is already registered in the databse.
    *
    * @param tx transaction to be used.
    * @param email of the invited person.
    * @return boolean telling if the user exists.
    */
  def checkIfUserExists (tx: Transaction, email: String): Future[Boolean] = for {
    result <- tx.execute(Json.obj(
      "statement" ->
        ( "MATCH (u:User {email: {email}}) "
        + "RETURN count(u)"),
      "parameters" -> Json.obj(
        "email" -> email
      )
    ))
  } yield if (result(0)("count(u)")(0) == JsNumber(1)) {
    true
  } else {
    false
  }

  /** Gets an invitation universal unique identifier if exists.
    *
    * @param tx transaction to be used.
    * @param user url-id of the user that is inviting.
    * @param email of the invited person.
    * @return An empty string if didn't found or the uuid string if found.
    */
  def invitationUUID (tx: Transaction, user: String, email: String): Future[String] = for {
    result <- tx.execute(Json.obj(
      "statement" ->
        ( "OPTIONAL MATCH (:User {url: {user}})-[:INVITES]->(i:InvitationPending {email: {email}}) "
        + "RETURN i.uuid"),
      "parameters" -> Json.obj(
        "user" -> user,
        "email" -> email
      )
    ))
  } yield if (result(0)("i.uuid")(0) == JsNull) {
    ""
  } else {
    result(0)("i.uuid")(0).as[String]
  }

  /** Sends an invitation email using sparkpost.com api.
    *
    * @param email of the invited person.
    * @param invitaion id.
    * @param message to be sent if the mail is successful.
    * @return JsObject with information about the success of the invitation.
    */
  def sendInvitationMail (email: String, invitation: String, message: String): Future[JsObject] = for {
    response <- WS.url("https://api.sparkpost.com/api/v1/transmissions").withHeaders(
      "Content-Type" -> "application/json",
      "Authorization" -> sparkpostApikey
    ).post(Json.obj(
      "options" -> Json.obj(
        "open_tracking" -> true,
        "click_tracking" -> true
      ),
      "recipients" -> Json.arr(Json.obj(
        "address" -> Json.obj(
          "email" -> email
        ),
        "tags" -> Json.arr("invitaion")
      )),
      "content" -> Json.obj(
        "from" -> Json.obj(
          "name" -> "Arckane",
          "email" -> "noreply@arckane.com"
        ),
        "subject" -> "Arckane Invitation!",
        "html" -> s"""
          <strong>Hello mage apprentice, you have just been invited to the Arckane beta test!</strong>
          <p>
            Arckane is a social and <a href="https://en.wikipedia.org/wiki/Crowdsourcing">crowdsourced</a>
            network, our objective is to make fun and easy to learn, share and use every possible skill in the world.
            In Arckane you can propose skills, relationships between them and
            resources for each skill, also hit like or dislike on skill relationships
            and resources. If you want to know how to develop any skill check how it is
            related to others and the best recources out there.
          </p>
          <p>
            Start your <a href="http://www.arckane.com?e=$email&i=$invitation">adventure by clicking here!</a>
          </p>
          <p>
            We hope you can master all the arcane spells of your own specialization,
            may it be in science, arts, humanities or a cool cocktail of those.
          </p>
          <p>
            <strong>Arckane.com</strong>
          </p>
        """
      )
    ))
  } yield if ((response.json \ "results" \ "total_accepted_recipients").as[Int] == 1)
    Json.obj("success" -> true, "message" -> message)
  else
    Json.obj("success" -> false, "message" -> ("Your invitation to "+email+" was not received."))

  /** Creates a unique universal identifier. */
  def uuid = java.util.UUID.randomUUID.toString
}
