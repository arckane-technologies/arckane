package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.system.{phi, pi, alpha}

package object decisions {

  trait Decision
  object DecisionTag extends Tag[Decision]("Decision")
  case class DecisionInfo (proposer: String, description: String, createdOn: Long = System.currentTimeMillis, finishedOn: Long = 0, executed: Boolean = false)

  implicit val userInfoWrites = new Writes[DecisionInfo] {
    def writes(props: DecisionInfo) = Json.obj(
      "proposer" -> props.proposer,
      "description" -> props.description,
      "createdOn" -> props.createdOn,
      "finishedOn" -> props.finishedOn,
      "executed" -> props.executed
    )
  }

  /*
   * Decision Ops.
   */

  implicit class DecisionOps[P] (decision: Arcklet[Decision, P]) {

    private def extractVotes (result: TxResult): Future[List[Int]] = Future {(
      (((result.head("uin.influence") \ "row").as[Seq[Int]] map (-_)) ++
      (result.head("udr.influence") \ "row").as[Seq[Int]]).toList
    )}

    def assert: Future[Boolean] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("""MATCH (d:Decision {url: {durl}}),
                                 (uin:User)-[:INFUSES]->(d),
                                 (udr:User)-[:DRAINS]->(d)
                           RETURN uin.influence, udr.influence"""),
        "parameters" -> Json.obj(
          "durl" -> decision.url
        )
      )
      votes <- extractVotes(result)
    } yield alpha(votes)
  }
}
