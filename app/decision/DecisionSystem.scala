package decision

import scala.concurrent.duration._

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._
import decision.skill._

package object system {

  /* Initial influence. */
  val phi: Int = 5

  /* A user needs more than this amount of influence to transfer influence. */
  val minInfluence: Int = 1

  /* Influence transfer. */
  def pi (t: (Int, Int)): (Int, Int) = t match {
    case (a, b) =>
      if (a <= 1) (a, b)
      else (a - 1, b + 1)
  }

  /* Influence transfer inverse. */
  def notPi (t: (Int, Int)): (Int, Int) = t match {
    case (a, b) =>
      if (a <= 1) (a, b)
      else (a - 1, b - 1)
  }

  /* Default time for each election for a decision. */
  implicit val electionsTime = 1.hour

  /* Less amount of users wont pass the decision. */
  implicit val minVoters = 10

  /* Commit threshold. */
  def alpha (votes: List[Int])(implicit mv: Int): Boolean = votes.length >= mv && votes.sum > 0

  val decisionMap: PartialFunction[(String, JsObject), Unit] = {
    case ("rename skill", args) => for {
      skill <- SkillTag get((args \ "skill").as[String])
      _ <- skill set("name", (args \ "name").as[String])
    } yield Unit
  }
}
