import scala.concurrent._
import scala.concurrent.duration._

import scalaz.Validation
import scalaz.Success
import scalaz.Failure

import org.specs2.mutable._
import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._
import play.api.libs.ws._

import utils.Database
import utils.Database._

class DatabaseSpecs extends Specification {

  var nodeId = 0

  "Database" should {

    "Database must be reachable" in new WithApplication {
      Await.result(Database.reachable, 2 seconds) match {
        case Success(response: WSResponse) => success
        case Failure(error: String) => ko(error)
      }
    }

    "Create node" in new WithApplication {
      import play.api.libs.concurrent.Execution.Implicits._
      Await.result( for {
        node <- createNode(Json.obj("foo" -> "baz"))
      } yield node, 2 seconds) match {
        case Success(cn: Node) =>
          nodeId = cn.id
          success
        case Failure(error: String) => ko(error)
      }
    }
    
  }
}
