package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

package object skill {

  trait Skill
  object SkillTag extends Tag[Skill]("Skill")

  implicit class SkillTagOps (tag: Tag[Skill]) {

    def getPageData (skillUrl: String): Future[Option[JsObject]] = for {
      result <- query(Json.arr(Json.obj(
        // Skill info
        "statement" -> ("MATCH (skill:"+tag.str+" {url: {urlmatcher}}) RETURN skill.name, skill.description"),
        "parameters" -> Json.obj(
          "urlmatcher" -> skillUrl
        )), Json.obj(
        // Related Skills
        "statement" -> ("MATCH (skill:"+tag.str+" {url: {urlmatcher}})-[:RELATED]->(s:Skill) RETURN s.name, s.url, s.description"),
        "parameters" -> Json.obj(
          "urlmatcher" -> skillUrl
        )), Json.obj(
        // Related Resources
        "statement" -> ("MATCH (skill:"+tag.str+" {url: {urlmatcher}})-[:HAS_RESOURCE]->(r:Resource) RETURN r.name, r.url, r.description, r.resourceType, r.resourceUrl"),
        "parameters" -> Json.obj(
          "urlmatcher" -> skillUrl
        ))))
    } yield if (result.length > 0) {
        Some(Json.obj(
          "name" -> result(0)("skill.name")(0).as[String],
          "description" -> result(0)("skill.description")(0).as[String],
          "related" -> (Json.toJson(result(1)("s.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(1)("s.url")(index),
            "resourceType" -> "skill",
            "infusionTarget" -> result(1)("s.url")(index),
            "description" -> result(1)("s.description")(index))
          }).as[JsArray] ++ Json.toJson(result(2)("r.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(2)("r.resourceUrl")(index),
            "resourceType" -> result(2)("r.resourceType")(index),
            "infusionTarget" -> result(2)("r.url")(index),
            "description" -> result(2)("r.description")(index))
          }).as[JsArray]))
        )
      } else {
        None
      }
  }
}
