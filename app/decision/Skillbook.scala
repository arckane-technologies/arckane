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
        "statement" -> ("MATCH (a)-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(n:Skill {url: {oldSkill}}), (m:Skill {url: {skill}})"
          + "CREATE (a)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(m) "
          + "DELETE r "
          + "WITH n "
          + "OPTIONAL MATCH (n)-[s:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {nextDepth}}]->(bs) "
          + "DELETE s "
          + "WITH collect(bs) as cbs "
          + "FOREACH (b IN cbs | MERGE (m:Skill {url: {skill}}) MERGE (m)-[:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {nextDepth}}]->(b)) "
        ),
        "parameters" -> Json.obj(
          "skill" -> skill,
          "oldSkill" -> oldSkill,
          "skillbook" -> skillbook,
          "depth" -> depth,
          "nextDepth" -> (depth + 1)
        ))
      ))
    } yield Unit

    def deleteSkill (skill: String, skillbook: String, depth: Int): Future[Unit] =
      if (depth == 3) for {
        _ <- query(Json.obj(
          "statement" -> ("MATCH ()-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 3}]->(:Skill {url: {skill}}) "
            + "DELETE r"),
          "parameters" -> Json.obj(
            "skill" -> skill,
            "skillbook" -> skillbook
          ))
        )
      } yield Unit
      else if (depth == 2) for {
        _ <- query(Json.obj(
          "statement" -> ("MATCH ()-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 2}]->(a:Skill {url: {skill}}) "
            + "OPTIONAL MATCH (a)-[s:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 3}]->(:Skill) "
            + "DELETE r,s"),
          "parameters" -> Json.obj(
            "skill" -> skill,
            "skillbook" -> skillbook
          ))
        )
      } yield Unit
    else if (depth == 1) for {
        _ <- query(Json.obj(
          "statement" -> ("MATCH ()-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 1}]->(a:Skill {url: {skill}}) "
            + "OPTIONAL MATCH (a)-[s:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 2}]->(b:Skill), "
            + "(b)-[t:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 3}]->(:Skill) "
            + "DELETE r,s,t"),
          "parameters" -> Json.obj(
            "skill" -> skill,
            "skillbook" -> skillbook
          ))
        )
      } yield Unit
      else Future(Unit)

    def deleteSkillbook (skillbook: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" -> ("MATCH (book:Skillbook {url: {skillbook}}) "
          + "OPTIONAL MATCH (book)-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 1}]->(a:Skill) "
          + "OPTIONAL MATCH (a)-[s:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 2}]->(b:Skill) "
          + "OPTIONAL MATCH (b)-[t:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: 3}]->(:Skill) "
          + "OPTIONAL MATCH (book)-[others]-() "
          + "DELETE r,s,t,book,others"),
        "parameters" -> Json.obj(
          "skillbook" -> skillbook
        ))
      )
    } yield Unit
  }
}
