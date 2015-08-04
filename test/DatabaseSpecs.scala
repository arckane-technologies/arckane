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

  var testNode: Node = null
  var testRelationship: Relationship = null

  "Database" should {

    "Database must be reachable" in new WithApplication {
      Await.result(reachable, 2 seconds) match {
        case Success(response: WSResponse) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Create node" in new WithApplication {
      import play.api.libs.concurrent.Execution.Implicits._
      Await.result(createNode(Json.obj("foo" -> "baz")), 2 seconds) match {
        case Success(createdNode: Node) =>
          testNode = createdNode
          success
        case Failure(error: Error) => ko(error)
      }
    }

    "Get node" in new WithApplication {
      Await.result(getNode(testNode.id), 2 seconds) match {
        case Success(obtainedNode: Node) => testNode must beEqualTo(obtainedNode)
        case Failure(error: Error) => ko(error)
      }
    }

    "Set a label" in new WithApplication {
      Await.result(addLabel(testNode, "Person"), 2 seconds) match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Update node properties" in new WithApplication {
      Await.result(updateNodeProperties(testNode, Json.obj("baz" -> "bar")), 2 seconds) match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Get node properties" in new WithApplication {
      Await.result(getNodeProperties(testNode), 2 seconds) match {
        case Success(properties: JsValue) => (properties \ "baz").validate[String] match {
          case s: JsSuccess[String] => s.get must beEqualTo("bar")
          case e: JsError => ko("Did not get a String from the path 'baz' with error: " + JsError.toJson(e).toString())
        }
        case Failure(error: Error) => ko(error)
      }
    }

    "Delete node" in new WithApplication {
      Await.result(deleteNode(testNode), 2 seconds) match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Create relationship" in new WithApplication {
      testNode = Await.result(createNode, 2 seconds).getOrElse(emptyNode)
      
    }

  }
}
