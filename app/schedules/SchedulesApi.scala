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

}
