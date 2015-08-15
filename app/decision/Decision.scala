package decision

import scala.concurrent.Future
import scala.concurrent.duration._

import akka.actor.ActorSystem
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import decision.system.{alpha, decisionMap}

package object decisions {

  trait Decision

  object DecisionTag extends Tag[Decision]("Decision")

  trait DecisionArgs

  object DecisionArgsTag extends Tag[DecisionArgs]("DecisionArgs")

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

  def startElection (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject])
    (implicit electionsTime: FiniteDuration, minVoters: Int, actorsystem: ActorSystem) = {
    val current = System.currentTimeMillis.milliseconds
    val created = manifest.props.createdOn.milliseconds
    val timeout = electionsTime + created - current
    actorsystem.scheduler.scheduleOnce(timeout) {
      for {
        passes <- manifest.assert
        _ <- if (passes) eleccionPasses(manifest, args)
             else eleccionFails(manifest, args)
      } yield Unit
    }
  }

  def eleccionPasses (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject]): Future[Unit] = for {
    _ <- manifest set(Json.obj("executed" -> true, "finishedOn" -> System.currentTimeMillis))
  } yield decisionMap(manifest.props.ident, args.props)

  def eleccionFails (manifest: Arcklet[Decision, DecisionManifest], args: Arcklet[DecisionArgs, JsObject]): Future[Unit] = for {
    _ <- manifest set("finishedOn", System.currentTimeMillis)
  } yield Unit

  /*
   * Decision Ops.
   */

  implicit class DecisionArckletOps[P] (decision: Arcklet[Decision, P]) {

    def assert (implicit minVoters: Int): Future[Boolean] = for {
      tx <- openTransaction
      result <- tx execute Json.arr(
        Json.obj(
          "statement" -> "MATCH (u:User)-[:INFUSES]->(d:Decision {url: {durl}}) RETURN u.influence",
          "parameters" -> Json.obj("durl" -> decision.url)),
        Json.obj(
          "statement" -> "MATCH (u:User)-[:DRAINS]->(d:Decision {url: {durl}}) RETURN u.influence",
          "parameters" -> Json.obj("durl" -> decision.url)))
      votes <- extractVotes(result)
    } yield alpha(votes)

    private def extractVotes (result: TxResult): Future[List[Int]] = Future(
      result(0)("u.influence").map(_.as[Int]) ++
      result(1)("u.influence").map(_.as[Int]).map(-_)
    )

    def delete: Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" -> "MATCH (d:Decision {url: {durl}}), (d)-[r]-(), (d)-[ra:WITH_ARGUMENTS]->(a:DecisionArgs) DELETE d, r, ra, a",
        "parameters" -> Json.obj("durl" -> decision.url)))
    } yield Unit
  }
}
