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

import arckane.db.persistence._
import users.user._

class UsersSpecs extends Specification {

  trait TestEntities extends Around with Scope {

    lazy val user = await(UserTag.create(Json.obj("description" -> "some description")))
    lazy val deleteUser = await(user.delete)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteUser
        }
      }
    }

    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
  }

  "Users service" should {

    "Set and get location" in new WithApplication with TestEntities {
      await(UserTag.setLocation(user.url, 1.5d, 1.5d))
      await(UserTag.getLocation(user.url)) must beEqualTo(Json.obj("lat" -> 1.5d, "lon" -> 1.5d))
    }

    "Return 'no such user' when trying to search location of unexistent user" in new WithApplication with TestEntities {
      await(UserTag.setLocation(user.url, 1.5d, 1.5d))
      await(UserTag.getLocation("oiahsih")) must beEqualTo(Json.obj("error" -> "no such user"))
    }

    "Set and get properties of a user" in new WithApplication with TestEntities {
      await(Arcklet(UserTag, user.url, Unit).set("some", "prop"))
      (await(UserTag.getProps(user.url))) must beEqualTo(Json.obj("url" -> user.url, "description" -> "some description", "some" -> "prop"))
    }
  }
}
