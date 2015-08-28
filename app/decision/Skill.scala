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

    def getPageData (skillUrl: String, skillbookUrl: String): Future[Option[Map[String, String]]] = for {
      result <- query(Json.arr(Json.obj(
        // Skill info
        "statement" -> ("MATCH (skill:"+tag.str+" {url: {urlmatcher}}) RETURN skill.name, skill.description"),
        "parameters" -> Json.obj(
          "urlmatcher" -> skillUrl
        )), Json.obj(
        // Book Info
        "statement" -> ("MATCH (book:Skillbook {url: {urlmatcher}}) RETURN book.name"),
        "parameters" -> Json.obj(
          "urlmatcher" -> skillbookUrl
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
    } yield if (result.length > 0)
        Some(Map(
          "name" -> result(0)("skill.name")(0).as[String],
          "url" -> skillUrl,
          "skillbookUrl" -> skillbookUrl,
          "skillbookName" -> result(1)("book.name")(0).as[String],
          "description" -> result(0)("skill.description")(0).as[String],
          "data" -> (Json.toJson(result(2)("s.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> (skillbookUrl + result(2)("s.url")(index).as[String]),
            "resourceType" -> "skill",
            "infusionTarget" -> result(2)("s.url")(index),
            "description" -> result(2)("s.description")(index))
        }).as[JsArray] ++ Json.toJson(result(3)("r.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> result(3)("r.resourceUrl")(index),
            "resourceType" -> result(3)("r.resourceType")(index),
            "infusionTarget" -> result(3)("r.url")(index),
            "description" -> result(3)("r.description")(index))
        }).as[JsArray]).toString))
      else
        None
  }
}
