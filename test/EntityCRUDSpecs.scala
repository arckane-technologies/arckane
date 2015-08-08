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
import utils.Persistent
import utils.PersistentOps._
import utils.ValidationOps._

class EntityCRUDSpecs extends Specification {

  trait TestEntities extends Around with Scope {

    lazy val user1 = Await.result(create(UserProps("user1@test.org", "pass1")), 2 seconds)
    lazy val user1Id = user1 match {
      case Success(user: User) => user.id
      case Failure(error: Err) => println(error); -1
    }
    lazy val getUser = Await.result(get[User](user1Id), 2 seconds)
    lazy val deleteUser1 = Await.result(delete(user1), 2 seconds)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteUser1
        }
      }
    }
  }

  "Entities CRUD opperations" should {

    "Create" in new WithApplication with TestEntities {
      user1 match {
        case Success(user: User) => success
        case Failure(error: Err) => ko(error.toString)
      }
    }

    "Get" in new WithApplication with TestEntities {
      getUser match {
        case Success(user: User) => user.props.email must beEqualTo("user1@test.org")
        case Failure(error: Err) => ko(error.toString)
      }
    }

    "Delete" in new WithApplication with TestEntities {
      deleteUser1 match {
        case Success(_) => success
        case Failure(error: Err) => ko(error.toString)
      }
    }
  }

  //"User influence system" should {}
}
