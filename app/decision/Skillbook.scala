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

    def getPageData (url: String): Future[Option[Map[String, String]]] = for {
      result <- query(Json.arr(Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {url: {urlmatcher}}) RETURN n.name, n.description"),
        "parameters" -> Json.obj(
          "urlmatcher" -> url
        )), Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {url: {urlmatcher}})-[:ROOT]->(s:Skill) RETURN s.name, s.url, s.description"),
        "parameters" -> Json.obj(
          "urlmatcher" -> url
        ))))
    } yield if (result.length > 0)
        Some(Map(
          "url" -> url,
          "name" -> result(0)("n.name")(0).as[String],
          "description" -> result(0)("n.description")(0).as[String],
          "data" -> Json.obj("related" -> Json.toJson(result(1)("s.name").zipWithIndex.map { case (name, index) => Json.obj(
            "name" -> name,
            "url" -> (url + result(1)("s.url")(index).as[String]),
            "infusionTarget" -> result(1)("s.url")(index),
            "content" -> result(1)("s.description")(index))
        })).toString))
      else
        None
  }
}
