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

    def getPageData (url: String, user: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" -> "MATCH (a:Skillbook {url: {url}}) OPTIONAL MATCH (:User {url: {user}})-[r:PROPOSES]->(a) RETURN a.name, r",
        "parameters" -> Json.obj(
          "url" -> url,
          "user" -> user
        ))
      )
    } yield if (result(0)("a.name").length > 0) {
        if (result(0)("r").length == 1)
          Some(Json.obj("name" -> result(0)("a.name")(0), "author" -> true))
        else
          Some(Json.obj("name" -> result(0)("a.name")(0), "author" -> false))
      } else {
        None
      }

    def getSubsection (source: String, skillbook: String, depth: Int): Future[JsArray] = for {
      result <- query(Json.obj(
        "statement" -> ("MATCH (a {url: {source}}) "
          + "OPTIONAL MATCH (a)-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(s:Skill) "
          + "RETURN s.name, s.url ORDER BY r.order"),
        "parameters" -> Json.obj(
          "source" -> source,
          "skillbook" -> skillbook,
          "depth" -> depth
        ))
      )
    } yield if (result(0)("s.name").length > 0) {
        val data = result(0)
        (data("s.name") zip data("s.url")).foldLeft(Json.arr()) { (array, skill) =>
          array :+ Json.obj("name" -> skill._1, "skillUrl" -> skill._2)
        }
      } else {
        Json.arr()
      }

    def changeName (skillbook: String, name: String, user: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" -> ("MATCH (a:Skillbook {url: {skillbook}})<-[:PROPOSES]-(:User {url: {user}})"
          + "SET a.name = {name}"),
        "parameters" -> Json.obj(
          "skillbook" -> skillbook,
          "name" -> name,
          "user" -> user
        ))
      )
    } yield Unit

    def addSkill (source: String, target: String, skillbook: String, depth: Int): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" -> ("MATCH (a {url: {source}}),(b {url: {target}})"
          + "CREATE (a)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(b)"),
        "parameters" -> Json.obj(
          "source" -> source,
          "target" -> target,
          "skillbook" -> skillbook,
          "depth" -> depth
        ))
      )
    } yield Unit

    def changeSkill (skill: String, oldSkill: String, skillbook: String, depth: Int): Future[Unit] = for {
      _ <- query(Json.arr(Json.obj(
        "statement" -> ("MATCH (n)-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(a:Skill {url: {oldSkill}}) "
          + "OPTIONAL MATCH (a)-[s:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {nextDepth}}]->(ss) "
          + "DELETE r,s "
          + "WITH n,ss "
          + "MATCH (b:Skill {url: {skill}}) "
          + "CREATE (n)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(b) "
          + "WITH b,collect(ss.url) AS subs "
          + "MATCH (sb:Skill) "
          + "WHERE sb.url IN subs "
          + "CREATE (b)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {nextDepth}}]->(sb)"
        ),
        "parameters" -> Json.obj(
          "skill" -> skill,
          "oldSkill" -> oldSkill,
          "skillbook" -> skillbook,
          "depth" -> depth,
          "nextDepth" -> (depth + 1).toString
        ))
      ))
    } yield Unit

    def removeSkill (skill: String, skillbook: String, depth: Int): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" -> ("MATCH (a:Skill {url: {skill}})<-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]-()"
          + "DELETE r"),
        "parameters" -> Json.obj(
          "skill" -> skill,
          "skillbook" -> skillbook,
          "depth" -> depth
        ))
      )
    } yield Unit
  }
}
