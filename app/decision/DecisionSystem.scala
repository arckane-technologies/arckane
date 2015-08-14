package decision

import play.api.libs.json._

import database.persistence._
import decision.skill._

package object system {

  /* Initial influence. */
  val phi: Int = 5

  /* Minimun amount of influence a user can have. */
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

  /* Milliseconds for each election for a decision. (1hr) */
  val electionsTime = 3600000

  /* Less amount of users wont pass the decision. */
  val minVoters = 10

  /* Commit threshold. */
  def alpha (votes: List[Int]): Boolean = votes.length > minVoters && votes.sum > 0

  val decisionMap: PartialFunction[(String, JsObject), Unit] = {
    case ("create skill", args) => Unit
  }
}
