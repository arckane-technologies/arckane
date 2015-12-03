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

import database.persistence._
import ontology.syntaxling._

class OntologySpecs extends Specification {

  trait TestEntities extends Around with Scope {

    val skill1Payload = Json.obj("title" -> "test", "web" -> "www.something.com")
    lazy val skill1 = await(SyntaxlingTag create skill1Payload)
    lazy val deleteSkill1 = await(skill1.delete)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
          deleteSkill1
        }
      }
    }

    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
  }

  "Syntaxlings" should {

    "Search skills in the database" in new WithApplication with TestEntities {
      skill1
      await(SyntaxlingTag.search("test")) must beEqualTo(Json.arr(Json.obj(
        "from_database" -> true,
        "title" -> "test",
        "web" -> "www.something.com"
      )))
    }

    "Search skills in Wikimedia if not found in the database" in new WithApplication with TestEntities {
      await(SyntaxlingTag.search("table"))
      true must beEqualTo(true)
    }
  }
}
