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

    lazy val user1 = Await.result(create(UserBasicInfo("user1@test.org")), 2 seconds)
    lazy val user2 = Await.result(create(UserBasicInfo("user2@test.org")), 2 seconds)
    lazy val user1Url = user1 match {
      case Success(user: User) => user.url
      case Failure(error: Err) => println(error); ""
    }
    lazy val user2Url = user2 match {
      case Success(user: User) => user.url
      case Failure(error: Err) => println(error); ""
    }
    lazy val setEmail2 = Await.result(set(user2, "email", JsString("user2@other.com")), 2 seconds)
    lazy val countUsers = Await.result(count[User], 2 seconds)
    lazy val getUser1 = Await.result(get[User, UserBasicInfo](user1Url), 2 seconds)
    lazy val getUser2 = Await.result(get[User, UserBasicInfo](user2Url), 2 seconds)
    lazy val deleteUser1 = Await.result(delete(user1), 2 seconds)
    lazy val deleteUser2 = Await.result(delete(user2), 2 seconds)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteUser1
          deleteUser2
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
      getUser1 match {
        case Success(User(props: UserBasicInfo, _, _)) => props.email must beEqualTo("user1@test.org")
        case Failure(error: Err) => ko(error.toString)
      }
    }

    "Set prop" in new WithApplication with TestEntities {
      setEmail2 match {
        case Success(_) => getUser2 match {
          case Success(User(props: UserBasicInfo, _, _)) => props.email must beEqualTo("user2@other.com")
          case Failure(error: Err) => ko(error.toString)
        }
        case Failure(error: Err) => ko(error.toString)
      }
    }

    "Count" in new WithApplication with TestEntities {
      user1
      user2
      countUsers match {
        case Success(n) => n must beEqualTo(2)
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
