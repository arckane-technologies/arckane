package decision

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.system.{alpha, electionsTime}

package object decisions {

  trait Decision
  object DecisionTag extends Tag[Decision]("Decision")

  case class DecisionManifest (
    ident: String,
    description: String,
    createdOn: Long = System.currentTimeMillis,
    finishedOn: Long = 0,
    executed: Boolean = false
  )

  implicit val decisionManifestWrites = new Writes[DecisionManifest] {
    def writes(props: DecisionManifest) = Json.obj(
      "ident" -> props.ident,
      "description" -> props.description,
      "createdOn" -> props.createdOn,
      "finishedOn" -> props.finishedOn,
      "executed" -> props.executed
    )
  }

  implicit val decisionManifestReads = (
    (JsPath \ "ident").read[String] and
    (JsPath \ "description").read[String] and
    (JsPath \ "createdOn").read[Long] and
    (JsPath \ "finishedOn").read[Long] and
    (JsPath \ "executed").read[Boolean]
  )(DecisionManifest.apply _)

  trait DecisionArgs
  object DecisionArgsTag extends Tag[DecisionArgs]("DecisionArgs")

  def startElection (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject])(implicit actorsystem: ActorSystem) = {
    val current: Long = System.currentTimeMillis
    val timeout: Long = electionsTime + manifest.props.createdOn - current
    actorsystem.scheduler.scheduleOnce(timeout.milliseconds) {
      for {
        passes <- manifest.assert
        _ <- if (passes) eleccionPasses(manifest, args)
             else eleccionFails(manifest, args)
      } yield Unit
    }
  }

  def eleccionPasses (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject]): Future[Unit] = for {
    _ <- Future(1)
  } yield Unit

  def eleccionFails (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject]): Future[Unit] = for {
    _ <- Future(1)
  } yield Unit

  /*
   * Decision Ops.
   */

  implicit class DecisionArckletOps[P] (decision: Arcklet[Decision, P]) {

    def assert: Future[Boolean] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("""MATCH (d:Decision {url: {durl}}),
                                 (infusers:User)-[:INFUSES]->(d),
                                 (drainers:User)-[:DRAINS]->(d)
                           RETURN infusers.influence, drainers.influence"""),
        "parameters" -> Json.obj(
          "durl" -> decision.url
        )
      )
      votes <- extractVotes(result)
    } yield alpha(votes)

    private def extractVotes (result: TxResult): Future[List[Int]] = Future(
      result("infusers.influence").map(_.as[Int]).map(-_) ++
      result("drainers.influence").map(_.as[Int])
    )
  }
}
