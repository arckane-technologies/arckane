/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.schedules

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.Node
import arckane.schedules.session._

/** Play Framework controller for the schedules service. */
class SchedulesApi extends Controller {

  def postSession = Action.async { request =>
    (for {
      user <- request.session.get("user-uri")
      response <- Some(sessionCreate(user))
    } yield response.map { uri =>
      Ok(Json.obj("uri" -> uri))
    }).getOrElse {
      Future(Unauthorized(Json.obj(
        "error" -> "you need an active session"
      )))
    }
  }

  def getSessionMentors = Action.async { request =>
    (for {
      user <- request.session.get("user-uri")
      response <- Some(sessionMentors(user))
    } yield response.map(Ok(_))).getOrElse {
      Future(Unauthorized(Json.obj(
        "error" -> "you need an active session"
      )))
    }
  }

  def getSession (uri: String) = Action.async { request =>
    Node.get(uri) map { optional =>
      optional match {
        case Some(session) => Ok(session)
        case None => NotFound("Session with uri "+uri+" not found.")
      }
    }
  }

  def putSession (uri: String) = Action.async(parse.json) { request =>
    (for {
      user <- request.session.get("user-uri")
      isOwner <- Some(sessionIsOwner(user, uri))
    } yield isOwner.flatMap(_ match {
      case true => Node.set(uri, request.body.as[JsObject]).map{_=> Ok}
      case false => Future(Forbidden("you don't own that resource"))
    })).getOrElse {
      Future(Unauthorized(Json.obj(
        "error" -> "you need an active session"
      )))
    }
  }

  def deleteSession (uri: String) = Action.async { request =>
    (for {
      user <- request.session.get("user-uri")
      isOwner <- Some(sessionIsOwner(user, uri))
    } yield isOwner.flatMap(_ match {
      case true => Node.delete(uri).map{_=> Ok}
      case false => Future(Forbidden("you don't own that resource"))
    })).getOrElse {
      Future(Unauthorized(Json.obj(
        "error" -> "you need an active session"
      )))
    }
  }
}
