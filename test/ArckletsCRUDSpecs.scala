import scala.concurrent._
import scala.concurrent.duration._

import scalaz.{Validation, Success, Failure}

import org.specs2.mutable._
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.Scope

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import play.api.libs.ws._

import database.persistence._
import decision.user._

class ArckletsCRUDSpecs extends Specification {

  trait TestEntities extends Around with Scope {

    lazy val user1 = await(UserTag createWith UserInfo("user1@test.org", "Franco"))
    lazy val user2 = await(UserTag createWith UserInfo("user2@test.org", "Grace"))
    lazy val setEmail2 = await(user2 set("email", "user2@other.com"))
    lazy val countUsers = await(UserTag.count)
    lazy val getUser1 = await(UserTag.getWith[UserInfo](user1.url))
    lazy val getUser2 = await(UserTag.getWith[UserInfo](user2.url))
    lazy val deleteUser1 = await(user1.delete)
    lazy val deleteUser2 = await(user2.delete)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteUser1
          deleteUser2
        }
      }
    }

    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
  }

  "Entities CRUD opperations" should {

    "Create" in new WithApplication with TestEntities {
      user1.props.name must beEqualTo("Franco")
    }

    "Get" in new WithApplication with TestEntities {
      getUser1.props.email must beEqualTo("user1@test.org")
    }

    "Set" in new WithApplication with TestEntities {
      setEmail2.props must beEqualTo("email" -> "user2@other.com")
    }

    "Count" in new WithApplication with TestEntities {
      user1
      user2
      countUsers must be_>(0)
    }

    "Delete" in new WithApplication with TestEntities {
      deleteUser1
      success
    }
  }
}
