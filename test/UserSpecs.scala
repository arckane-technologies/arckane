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

import utils.Database._
import decision.User
import decision.UserOps._

class UserSpecs extends Specification {

  trait TestUsers extends Around with Scope {

    import decision.DecisionSystem._
    lazy val user1 = Await.result(createUser("user1@test.org", "pass1"), 2 seconds)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          // delete users and relationships
        }
      }
    }
  }

  "User basic opperations" should {

    "Create user" in new WithApplication with TestUsers {
      user1 match {
        case Success(user: User) => println(user); success
        case Failure(error: Error) => ko(error)
      }
    }
  }

  //"User influence system" should {}
}
