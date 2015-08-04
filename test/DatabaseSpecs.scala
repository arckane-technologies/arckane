import scala.concurrent._
import scala.concurrent.duration._

import scalaz.Validation
import scalaz.Success
import scalaz.Failure

import org.specs2.mutable._
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.Scope

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._
import play.api.libs.ws._

import utils.Database
import utils.Database._

class DatabaseSpecs extends Specification {

  trait TestNode extends Around with Scope {

    lazy val isReachable = Await.result(reachable, 2 seconds)
    lazy val testNode = Await.result(createNode(Json.obj("foo" -> "baz")), 2 seconds)
    lazy val deleteTestNode = {
      deleteTestRelationship
      afterNodeCreation {n: Node => Await.result(deleteNode(n), 2 seconds)}
    }
    lazy val reobtainedNode = afterNodeCreation {n: Node => Await.result(getNode(n.id), 2 seconds)}
    lazy val labeled = afterNodeCreation {n: Node => Await.result(addLabel(n, "Person"), 2 seconds)}
    lazy val updatedProperties = afterNodeCreation {n: Node => Await.result(updateNodeProperties(n, Json.obj("baz" -> "bar")), 2 seconds)}
    lazy val nodeProperties = afterNodeCreation {n: Node => Await.result(getNodeProperties(n), 2 seconds)}
    lazy val testRelationship = afterNodeCreation {n: Node => Await.result(createRelationship(n, n, "TEST"), 2 seconds)}
    lazy val deleteTestRelationship = afterRelationshipCreation {r: Relationship => Await.result(deleteRelationship(r), 2 seconds)}

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteTestRelationship
          deleteTestNode
        }
      }
    }

    def afterNodeCreation[A] (f: Node => Validation[Error, A]): Validation[Error, A] = testNode match {
      case Success(node) => f(node)
      case e: Failure[Error] => Failure("Node creation failed beforehand")
    }

    def afterRelationshipCreation[A] (f: Relationship => Validation[Error, A]) = testRelationship match {
      case Success(relationiship) => f(relationiship)
      case e: Failure[Error] => Failure("Relatonship creation failed beforehand")
    }

  }

  "Database" should {

    "Database must be reachable" in new WithApplication with TestNode {
      isReachable match {
        case Success(response: WSResponse) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Create node" in new WithApplication with TestNode {
      testNode match {
        case Success(createdNode: Node) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Get node" in new WithApplication with TestNode {
      reobtainedNode match {
        case Success(obtainedNode: Node) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Set a label" in new WithApplication with TestNode {
      labeled match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Update node properties" in new WithApplication with TestNode {
      updatedProperties match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Get node properties" in new WithApplication with TestNode {
      nodeProperties match {
        case Success(properties: JsValue) => (properties \ "foo").validate[String] match {
          case s: JsSuccess[String] => s.get must beEqualTo("baz")
          case e: JsError => ko("Did not get a String from the path 'baz' with error: " + JsError.toJson(e).toString())
        }
        case Failure(error: Error) => ko(error)
      }
    }

    "Delete node" in new WithApplication with TestNode {
      deleteTestNode match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Create relationship" in new WithApplication with TestNode {
      testRelationship match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

    "Delete relationship" in new WithApplication with TestNode {
      deleteTestRelationship match {
        case Success(_) => success
        case Failure(error: Error) => ko(error)
      }
    }

  }
}
