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
import decision.system._
import decision.user._

class InfluenceSpecs extends Specification {

  trait TestInfluence extends Around with Scope {

    import play.api.libs.concurrent.Execution.Implicits._

    lazy val t1States = Await.result( for {
      user1 <- UserTag.create
      user2 <- UserTag.create
      user3 <- UserTag.create
      // u1 = 4, u2 = 6
      _ <- user1 infuseUser user2
      // u2 = 5, u1 = 3
      _ <- user2 drainUser user1
      // u3 = 4, u1 = 2
      _ <- user3 drainUser user1
      // u1 = 1, u3 = 5
      _ <- user1 infuseUser user3
      // u1 = 1, u2 = 5
      _ <- user1 infuseUser user2
      // u1 = 1, u2 = 5
      _ <- user1 infuseUser user2
      u1i <- user1.get[Int]("influence")
      u2i <- user2.get[Int]("influence")
      u3i <- user3.get[Int]("influence")
      _ <- user1.delete
      _ <- user2.delete
      _ <- user3.delete
    } yield (u1i, u2i, u3i), 5 seconds)

    abstract override def around[T: AsResult](t: => T): Result = {
      super.around {
        try t // Execute test
        finally {
        }
      }
    }

    def await[A] (ft: Future[A]): A = Await.result(ft, 2 seconds)
  }

  "The influence system" should {

    "Infusion and draining" in new WithApplication with TestInfluence {
      t1States must beEqualTo((1, 5, 5))
    }
  }
}
