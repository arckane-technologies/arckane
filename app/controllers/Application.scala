/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package controllers

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._
import arckane.schedules.session._

/** Play Framework controller for everything related to Arckane's app serving and authentication. */
class Application extends Controller {

  def getSession (request: Request[AnyContent]): Option[String] = for {
    name <- request.session.get("user-name")
    uri <- request.session.get("user-uri")
  } yield Json.obj(
    "user-name" -> name,
    "user-uri" -> uri
  ).toString

  def guest: String = Json.obj("guest" -> true).toString

  def isGuest (request: Request[AnyContent]): Boolean = request.session.get("user-name") match {
    case Some(name) => false
    case None => true
  }

  def index = Action { request =>
    getSession(request) match {
      case Some(session) => Ok(views.html.index(session))
      case None => Ok(views.html.index(guest))
    }
  }

  def signin = Action { request =>
    if (isGuest(request)) Ok(views.html.signin(guest))
    else Redirect("/")
  }

  def signup = Action { request =>
    if (isGuest(request)) Ok(views.html.signup(guest))
    else Redirect("/")
  }

  def signout = Action {
    Redirect("/").withNewSession
  }

  def profile (uri: String) = Action { request =>
    getSession(request) match {
      case Some(session) => Ok(views.html.profile(session, uri))
      case None => Ok(views.html.profile(guest, uri))
    }
  }

  def mentor = Action { request =>
    getSession(request) match {
      case Some(session) => Ok(views.html.mentor(session))
      case None => Redirect("/")
    }
  }

  def editSession (uri: String) = Action.async { request =>
    (for {
      user <- request.session.get("user-uri")
      userSession <- getSession(request)
      response <- Some(sessionIsOwner(user, uri))
    } yield response.map { isOwner =>
      if (isOwner) Ok(views.html.editSession(userSession, uri))
      else Redirect("/")
    }).getOrElse {
      Future(Redirect("/"))
    }
  }
}
