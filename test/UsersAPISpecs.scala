/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

import scala.concurrent._
import scala.concurrent.duration._

import org.specs2.mutable._
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.Scope

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import play.api.libs.ws._

import arckane.db.transaction._
import arckane.db.Node

class UsersAPISpecs extends Specification {

  "Users API endpoint" should {

    var userUri = ""

    "POST /api/users/signup : 400 (bad json object)" in new WithApplication {
      val Some(result) = route(FakeRequest(POST, "/api/users/signup").withJsonBody(
        Json.obj("bad" -> "json")
      ))
      status(result) must equalTo(BAD_REQUEST)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("error" -> "bad json object"))
    }

    "POST /api/users/signup : 200" in new WithApplication {
      val Some(result) = route(FakeRequest(POST, "/api/users/signup").withJsonBody(
        Json.obj(
          "firstname" -> "Someone",
          "lastname" -> "Else",
          "email" -> "test@mail.com",
          "password" -> "root"
        )
      ))
      userUri = session(result).get("user-uri").getOrElse{""}
      status(result) must equalTo(OK)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("success" -> true))
      (for {
        uri <- session(result).get("user-uri")
        name <- session(result).get("user-name")
      } yield name == "Someone" && uri.length > 0) must beSome(true)
    }

    "POST /api/users/signin : 400 (bad json object)" in new WithApplication {
      val Some(result) = route(FakeRequest(POST, "/api/users/signin").withJsonBody(
        Json.obj("bad" -> "json")
      ))
      status(result) must equalTo(BAD_REQUEST)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("error" -> "bad json object"))
    }

    "POST /api/users/signin : 200 (with unsuccessful authentication)" in new WithApplication {
      val Some(result) = route(FakeRequest(POST, "/api/users/signin").withJsonBody(
        Json.obj(
          "email" -> "test@mail.com",
          "password" -> "bad"
        )
      ))
      status(result) must equalTo(OK)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("success" -> false))
      (for {
        uri <- session(result).get("user-uri")
        name <- session(result).get("user-name")
      } yield name) must beNone
    }

    "POST /api/users/signin : 200 (with successful authentication)" in new WithApplication {
      val Some(result) = route(FakeRequest(POST, "/api/users/signin").withJsonBody(
        Json.obj(
          "email" -> "test@mail.com",
          "password" -> "root"
        )
      ))
      status(result) must equalTo(OK)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("success" -> true))
      (for {
        uri <- session(result).get("user-uri")
        name <- session(result).get("user-name")
      } yield name == "Someone" && uri.length > 0) must beSome(true)
    }

    "DELETE /api/users/self : 401 (with no active session)" in new WithApplication {
      val Some(result) = route(FakeRequest(DELETE, "/api/users/self"))
      status(result) must equalTo(UNAUTHORIZED)
      contentType(result) must beSome("application/json")
      contentAsJson(result) must equalTo(Json.obj("error" -> "you need an active session"))
    }

    "DELETE /api/users/self : 200" in new WithApplication {
      val Some(result) = route(FakeRequest(DELETE, "/api/users/self").withSession("user-uri" -> userUri))
      status(result) must equalTo(OK)
    }
  }
}
