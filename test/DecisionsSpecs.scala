import scala.concurrent._
import scala.concurrent.duration._

import org.specs2.mutable._
import org.specs2.execute.AsResult
import org.specs2.execute.Result
import org.specs2.specification.Scope

import play.api.test._
import play.api.test.Helpers._
import play.api.libs.json._

import database.persistence._
import decision.user._
import decision.skill._
import decision.decisions._

class DecisionsSpecs extends Specification {
  "The decision system" should {

    "Passing decision" in new WithApplication {
      import play.api.libs.concurrent.Execution.Implicits._

      implicit val minVotrers = 4
      implicit val electionTime = 1.seconds
      implicit val actorSystem = play.api.libs.concurrent.Akka.system

      val skillName = Await.result(for {
        user1 <- UserTag.create
        user2 <- UserTag.create
        user3 <- UserTag.create
        user4 <- UserTag.create
        skill <- SkillTag.create(Json.obj("name" -> "func prog"))
        _ <- user1 infuseUser user2
        proposal1 <- user2 propose(
          "rename skill",
          "We should use the complete name of the skill",
          Json.obj("skill" -> skill.url, "name" -> "Functional Programming")
        )
        _ <- user1 drain proposal1
        _ <- user2 infuse proposal1
        _ <- user3 infuse proposal1
        _ <- user4 drain proposal1
        _ <- Future { Thread.sleep(1000) }
        _ <- proposal1.delete
        name <- skill.get[String]("name")
        _ <- skill.delete
        _ <- user1.delete
        _ <- user2.delete
        _ <- user3.delete
        _ <- user4.delete
      } yield name , 2 seconds)

      skillName must beEqualTo("Functional Programming")
    }

    "Not passing decision" in new WithApplication {
      import play.api.libs.concurrent.Execution.Implicits._

      implicit val minVotrers = 4
      implicit val electionTime = 1.seconds
      implicit val actorSystem = play.api.libs.concurrent.Akka.system

      val skillName = Await.result(for {
        user1 <- UserTag.create
        user2 <- UserTag.create
        user3 <- UserTag.create
        user4 <- UserTag.create
        skill <- SkillTag.create(Json.obj("name" -> "Biology"))
        _ <- user1 infuseUser user2
        proposal1 <- user2 propose(
          "rename skill",
          "We should be noobs",
          Json.obj("skill" -> skill.url, "name" -> "Bios")
        )
        _ <- user1 infuse proposal1
        _ <- user2 drain proposal1
        _ <- user3 infuse proposal1
        _ <- user4 drain proposal1
        _ <- Future { Thread.sleep(1000) }
        _ <- proposal1.delete
        name <- skill.get[String]("name")
        _ <- skill.delete
        _ <- user1.delete
        _ <- user2.delete
        _ <- user3.delete
        _ <- user4.delete
      } yield name, 2 seconds)

      skillName must beEqualTo("Biology")
    }
  }
}
