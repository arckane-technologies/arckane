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

import decision.UserOps._
import utils.DatabaseOps.Node
import utils.ValidationOps._

class UserSpecs extends Specification {

  trait TestUsers extends Around with Scope {

    import decision.SimpleLogarithmicCommunity._
    lazy val user1 = Await.result(createUser("user1@test.org", "pass1"), 2 seconds)
    lazy val user1Id = user1 match {
      case Success(user: User) => user.id
      case Failure(error: Err) => println(error); -1
    }

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          Await.result(deleteUser(user1), 2 seconds)
        }
      }
    }
  }

  "User basic opperations" should {

    "Create user" in new WithApplication with TestUsers {
      user1 match {
        case Success(user: User) => success
        case Failure(error: Err) => ko(error.toString)
      }
    }

    "Get user" in new WithApplication with TestUsers {
      Await.result(getUser(user1Id), 2 seconds) match {
        case Success(user: User) => user.id must beEqualTo(user1Id)
        case Failure(error: Err) => ko(error.toString)
      }
    }
  }

  //"User influence system" should {}
}
