/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.users

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.Node
import arckane.users.user._

class UsersApi extends Controller {

  def signup = Action.async(parse.json) { request =>
    val json = request.body.as[JsObject]
    (for {
      firstname <- (json \ "firstname").asOpt[String]
      lastname <- (json \ "lastname").asOpt[String]
      email <- (json \ "email").asOpt[String]
      password <- (json \ "password").asOpt[String]
    } yield userCreate(firstname, lastname, email, password).map { uri =>
      Ok(Json.obj(
        "success" -> true
      )).withSession(
        "user-uri" -> uri,
        "user-name" -> firstname
      )
    }).getOrElse {
      Future(BadRequest(Json.obj(
        "error" -> "bad json object"
      )))
    }
  }

  def signin = Action.async(parse.json) { request =>
    val json = request.body.as[JsObject]
    (for {
      email <- (json \ "email").asOpt[String]
      password <- (json \ "password").asOpt[String]
    } yield authenticate(email, password).map {
      case Some((uri, name)) =>
        Ok(Json.obj(
          "success" -> true
        )).withSession(
          "user-uri" -> uri,
          "user-name" -> name
        )
      case None =>
        Ok(Json.obj(
          "success" -> false
        ))
    }).getOrElse {
      Future(BadRequest(Json.obj(
        "error" -> "bad json object"
      )))
    }
  }

  def deleteUser = Action.async { request =>
    request.session.get("user-uri").map { uri =>
      userDelete(uri).map {_=>Ok}
    }.getOrElse {
      Future(Unauthorized(Json.obj(
        "error" -> "you need an active session"
      )))
    }
  }
}
