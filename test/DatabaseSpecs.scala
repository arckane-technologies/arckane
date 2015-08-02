import scala.concurrent._
import scala.concurrent.duration._

import scalaz._

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.ws._

import utils.Database

class DatabaseSpecs extends Specification {

  "Database" should {

    "Database must be reachable" in new WithApplication {
      Await.result(Database.reachable, 2 seconds) match {
        case Success(response: WSResponse) => success
        case Failure(error: String) => ko(error)
      }
    }
  }
}
