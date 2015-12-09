/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package schedules

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import schedules.session._

/** Play Framework controller for the schedules service. */
class SchedulesApi extends Controller {

  def getSessionInfo (sessionId: String) = Action.async { request =>
    val viewer = request.session.get("home") match {
      case Some(url) => url
      case None => ""
    }
    SessionTag.getSessionInfo("/session/"+sessionId, viewer).map { session =>
      session match {
        case Some(session) => Ok(session)
        case None => NotFound("Not such session id.")
      }
    }
  }

  def getMentorSessions = Action.async { request =>
    (for {
      user <- request.session.get("home")
      response <- Some(SessionTag.retriveMentorSessions(user))
    } yield response.map { sessions =>
      Ok(sessions)
    }).getOrElse {
      Future(Redirect("/"))
    }
  }

  def deleteSession (sessionId: String) = Action.async { request =>
    (for {
      user <- request.session.get("home")
      response <- Some(SessionTag.deleteSession("/session/"+sessionId, user))
    } yield response.map { Unit =>
      Redirect("/mentor")
    }).getOrElse {
      Future(BadRequest("You need an active session."))
    }
  }

  def createSession = Action.async { request =>
    (for {
      user <- request.session.get("home")
      response <- Some(SessionTag.createSession(user))
    } yield response.map { arcklet =>
      Ok(Json.obj("sessionId" -> arcklet.url))
    }).getOrElse {
      Future(BadRequest("You need an active session."))
    }
  }

  def getSessionEditData (sessionId: String) = Action.async { request =>
    SessionTag.getSessionEditData("/session/"+sessionId).map { session =>
      session match {
        case Some(session) => Ok(session)
        case None => NotFound("Not such session id.")
      }
    }
  }

  def setProp (sessionId: String) = Action.async { request =>
    val numeric = request.queryString.get("numeric") match {
      case Some(_) => true
      case None => false
    }
    (for {
      user <- request.session.get("home")
      prop <- request.queryString.get("prop")
      value <- request.queryString.get("value")
      isOwner <- Some(SessionTag.isOwner(user, "/session/"+sessionId))
    } yield isOwner.flatMap { isOwner =>
      if (isOwner) {
        if (numeric) {
          Arcklet(SessionTag, "/session/"+sessionId, Unit).set(prop.head, value.head.toDouble).map { _ => Ok }
        } else {
          Arcklet(SessionTag, "/session/"+sessionId, Unit).set(prop.head, value.head).map { _ => Ok }
        }
      } else {
        Future(BadRequest)
      }
    }).getOrElse {
      Future(BadRequest("Expected user, prop and value in the query string."))
    }
  }

}
