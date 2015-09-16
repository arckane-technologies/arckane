package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

package object skillbook {

  trait Skillbook
  object SkillbookTag extends Tag[Skillbook]("Skillbook")

  implicit class SkillbookTagOps (tag: Tag[Skillbook]) {

    def getPageData (url: String): Future[Option[JsObject]] = for {
      result <- query(Json.arr(Json.obj(
        "statement" -> "MATCH (a:Skillbook {url: {url}}) RETURN a.name",
        "parameters" -> Json.obj(
          "url" -> url
        ))
      ))
    } yield if (result(0)("a.name").length > 0) {
        Some(Json.obj("name" -> result(0)("a.name")(0)))
      } else {
        None
      }

    def getSubsection (source: String, skillbook: String, depth: Int): Future[JsArray] = for {
      result <- query(Json.arr(Json.obj(
        "statement" -> ("MATCH (a {url: {source}}) "
          + "OPTIONAL MATCH (a)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(s:Skill) "
          + "RETURN s.name, s.url"),
        "parameters" -> Json.obj(
          "source" -> source,
          "skillbook" -> skillbook,
          "depth" -> depth
        ))
      ))
    } yield if (result(0)("s.name").length > 0) {
        val data = result(0)
        (data("s.name") zip data("s.url")).foldLeft(Json.arr()) { (array, skill) =>
          array :+ Json.obj("name" -> skill._1, "skillUrl" -> skill._2)
        }
      } else {
        Json.arr()
      }
  }
}
