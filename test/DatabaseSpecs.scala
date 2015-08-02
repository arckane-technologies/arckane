import scala.concurrent._
import scala.concurrent.duration._

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._

import utils.Database

class DatabaseSpecs extends Specification {

  "Database" should {

    "Ping to the query path" in new WithApplication {
      Await.result(Database.ping, 2 seconds) must beEqualTo(true)
    }
  }
}
