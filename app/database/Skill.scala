/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package database

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of Skill. */
package object skill {

  /** Skill type. */
  trait Skill

  /** Data type for Neo4j :Skill tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SkillTag extends Tag[Skill]("Skill")

  /** Type class for the [[SkillTag]] data type. */
  implicit class SkillTagOps (tag: Tag[Skill]) {

    /** Creates a new skill proposed by a user.
      *
      * @param name of the skill to be added to the database.
      * @param description of the skill to be added to the database.
      * @param user url-id of the user that is creating/proposing the skill.
      * @return a json object with the url-id of the created skill. {url: string}
      */
    def propose (name: String, description: String, user: String): Future[JsObject] = for {
      tx <- openTransaction
      skill <- SkillTag.create(tx, Json.obj(
        "name" -> name.clean.trim,
        "description" -> description.trim,
        "resourceType" -> "skill"))
      _ <- tx.lastly(Json.obj(
        "statement" -> ("MATCH (u:User {url:{user}}),(s:Skill {url:{skill}}) CREATE (u)-[:PROPOSES]->(s)"),
        "parameters" -> Json.obj(
          "user" -> user,
          "skill" -> skill.url
        )))
    } yield Json.obj("url" -> skill.url)

    /** Searches through the database for a skill with a similar name to the provided
      * query string, returns an array of possible skills, with its name and url.
      *
      * @param searchString the string to search for.
      * @return a json array with the matching skills. [{name: string, url: string}]
      */
    def search (searchString: String): Future[JsArray] = { for {
      result <- query(Json.obj(
        "statement" -> "MATCH (n:Skill) WHERE n.name =~ { regex } RETURN n.name, n.url",
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+searchString.clean.escapeParenthesis+".*")
        )))
      } yield if (result(0)("n.url").length > 0) {
        val data = (result(0)("n.url") zip result(0)("n.name"))
        data.foldLeft(Json.arr()) { (array, data) =>
          array :+ Json.obj("url" -> data._1, "name" -> data._2)
        }
      } else {
        Json.arr()
      }
    }

    /** Retrieves from the database all the data needed to display the skill page,
      * including an array of related other skills and external resources.
      *
      * @param skillUrl url-id of the skill that will be displayed.
      * @return a json object with all the data. {name: string, description: string, related: Array<Resource>}
      */
    def getPageData (skillUrl: String): Future[Option[JsObject]] = { for {
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
}
