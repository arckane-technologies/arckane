/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

import scala.concurrent._
import scala.concurrent.duration._

import org.specs2.mutable._
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.Scope

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import play.api.libs.ws._

import arckane.db.transaction._
import arckane.db.Tag

class DatabaseSpecs extends Specification {

  trait TestEntities extends Around with Scope {

    //val skill1Payload = Json.obj("title" -> "test", "web" -> "www.something.com")
    //lazy val skill1 = await(SyntaxlingTag create skill1Payload)
    //lazy val deleteSkill1 = await(skill1.delete)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          //deleteSkill1
        }
      }
    }

    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
  }

  "Transactions" should {

    "Execute and respond one map for one query" in new WithApplication with TestEntities {
      import play.api.libs.concurrent.Execution.Implicits._

      val uri = await(Tag.create(Json.obj(
        "name" -> "Franco"
      ))("Person"))
      await(Tag.set(uri, Json.obj(
        "name" -> "Francisco",
        "lastname" -> "Aramburo"
      )))
      await(Tag.set(uri, Json.obj(
        "lastname" -> "Ara"
      )))
      println(await(Tag.get(uri)))
      await(Tag.delete(uri))
      true must beEqualTo(true)
    }
  }
}
