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

/** Data types and type classes of Skillbook. */
package object skillbook {

  /** Skillbook type. */
  trait Skillbook

  /** Data type for Neo4j :Skillbook tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SkillbookTag extends Tag[Skillbook]("Skillbook")

  /** Type class for the [[SkillbookTag]] data type. */
  implicit class SkillbookTagOps (tag: Tag[Skillbook]) {

    /** Creates a new empty skillbook.
      *
      * @param user url-id of the user that is proposing the skillbook.
      * @return a json object with the created skillbook url. {url: string}
      */
    def propose (user: String): Future[JsObject] = for {
      tx <- openTransaction
      skillbook <- tag.create(tx, Json.obj("name" -> "New Skillbook", "description" -> "Skills for the adept mage!"))
      _ <- tx.lastly(Json.obj(
        "statement" ->
          ( "MATCH (s:Skillbook {url: {skillbook}}),(u:User {url: {user}}) "
          + "CREATE (u)-[:PIN]->(s)<-[:PROPOSES]-(u)"),
        "parameters" -> Json.obj(
          "skillbook" -> skillbook.url,
          "user" -> user
        )))
    } yield Json.obj("url" -> skillbook.url)

    /** Retrives basic data from the database about a skillbook.
      *
      * @param url url-id of the skillbook to be retrived.
      * @param user url-id of the user that is requesting to see if is the author.
      * @return a json object with the name of the skillbook and if the user is the
      * author {name: string, author: boolean} wraped in an Option to see if found.
      */
    def getPageData (url: String, user: String): Future[Option[JsObject]] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (a:Skillbook {url: {url}}) "
          + "OPTIONAL MATCH (:User {url: {user}})-[r:PROPOSES]->(a) "
          + "RETURN a.name, a.description, r"),
        "parameters" -> Json.obj(
          "url" -> url,
          "user" -> user
        ))
      )
    } yield if (result(0)("a.name").length > 0) {
        if (result(0)("r")(0) != JsNull)
          Some(Json.obj(
            "name" -> result(0)("a.name")(0),
            "description" -> result(0)("a.description")(0),
            "author" -> true
          ))
        else
          Some(Json.obj(
            "name" -> result(0)("a.name")(0),
            "description" -> result(0)("a.description")(0),
            "author" -> false
          ))
      } else {
        None
      }

    /** Retrieves all the skillbooks that the user has pinned.
      *
      * @param user url-id of the user that wants to retrive his skillbooks.
      * @return a json array with the skillbooks. [{name: string, url: string}]
      */
    def userSkillbooks (user: String): Future[JsArray] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (u:User {url: {user}}) "
          + "OPTIONAL MATCH (u)-[:PIN]->(s:Skillbook) "
          + "RETURN s ORDER BY s.name"),
        "parameters" -> Json.obj(
          "user" -> user
        ))
      )
    } yield if (result(0)("s")(0) != JsNull) {
      Json.toJson(result(0)("s")).as[JsArray]
    } else {
      Json.arr()
    }

    /** Retrives data about a subsection of a skillbook from the databse.
      *
      * @param source url-id of the skill, or father skill of the subsection.
      * @param skillbook url-id of the related skillbook.
      * @param depth of the subsection that is being retrieved.
      * @return json array with the skills of the subsection. [{name: string, skillUrl: string}]
      */
    def getSubsection (source: String, skillbook: String, depth: Int): Future[JsArray] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (a {url: {source}}) "
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

    /** Queries the database to change a skillbook name.
      *
      * @param skillbook url-id of the skillbook that is going to be edited.
      * @param name to be set.
      * @param user url-id that wants to change the name of his skillbook.
      * @return Unit
      */
    def changeName (skillbook: String, name: String, user: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" ->
          ( "MATCH (a:Skillbook {url: {skillbook}})<-[:PROPOSES]-(:User {url: {user}}) "
          + "SET a.name = {name}"),
        "parameters" -> Json.obj(
          "skillbook" -> skillbook,
          "name" -> name,
          "user" -> user
        ))
      )
    } yield Unit

    /** Queries the database to change a skillbook description.
      *
      * @param skillbook url-id of the skillbook that is going to be edited.
      * @param description to be set.
      * @param user url-id that wants to change the description of his skillbook.
      * @return Unit
      */
    def changeDescription (skillbook: String, description: String, user: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" ->
          ( "MATCH (a:Skillbook {url: {skillbook}})<-[:PROPOSES]-(:User {url: {user}}) "
          + "SET a.description = {description}"),
        "parameters" -> Json.obj(
          "skillbook" -> skillbook,
          "description" -> description,
          "user" -> user
        ))
      )
    } yield Unit

    /** Adds a skill to a subsection of a skillbook.
      *
      * @param source url-id of the skill, or father skill of the subsection.
      * @param target url-id of the skill that is going to be added.
      * @param skillbook url-id of the related skillbook.
      * @param depth of the subsection.
      * @return Unit
      */
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

    /** Changes a skill of a subsection in a skillbook.
      *
      * @param skill url-id to be added.
      * @param oldSkill url-id to be removed.
      * @param skillbook url-id of the related skillbook.
      * @param depth of the subsection.
      * @return Unit
      */
    def changeSkill (skill: String, oldSkill: String, skillbook: String, depth: Int): Future[Unit] = for {
      _ <- query(Json.arr(Json.obj(
        "statement" ->
          ( "MATCH (a)-[r:SKILLBOOK_DEPTH {skillbook: {skillbook}, depth: {depth}}]->(n:Skill {url: {oldSkill}}), (m:Skill {url: {skill}}) "
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

    /** Toggles a relationship between a user and a skillbook known as :PIN.
      *
      * @param user url-id that is pinning.
      * @param skillbook url-id that is being pinned.
      * @return a json object telling if the pin was disabled or enabled. {pinned: boolean}
      */
    def pinToggle (user: String, skillbook: String): Future[JsObject] = for {
        result <- query(Json.obj(
          "statement" ->
            ( "OPTIONAL MATCH (:User {url: {user}})-[r:PIN]->(:Skillbook {url: {skillbook}}) "
            + "RETURN r"),
          "parameters" -> Json.obj(
            "user" -> user,
            "skillbook" -> skillbook
          )))
        _ <- if (result(0)("r")(0) == JsNull) {
          query(Json.obj(
            "statement" ->
              ( "MATCH (u:User {url: {user}}),(s:Skillbook {url: {skillbook}}) "
              + "CREATE (u)-[:PIN]->(s)"),
            "parameters" -> Json.obj(
              "user" -> user,
              "skillbook" -> skillbook
            )
          ))
        } else {
          query(Json.obj(
            "statement" ->
              ( "MATCH (:User {url: {user}})-[r:PIN]->(:Skillbook {url: {skillbook}}) "
              + "DELETE r"),
            "parameters" -> Json.obj(
              "user" -> user,
              "skillbook" -> skillbook
            )
          ))
        }
    } yield if (result(0)("r")(0) == JsNull)
      Json.obj("pinned" -> true)
    else
      Json.obj("pinned" -> false)

    /** Removes a skill from a subsection of a skillbook.
      *
      * @param skill url-id to be deleted.
      * @param skillbook url-id of the related skillbook.
      * @param depth of the subsection.
      * @return Unit
      */
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

    /** Deletes a skillbook from the database.
      *
      * @param skillbook url-id that is going to be deleted.
      * @return Unit
      */
    def delete (skillbook: String): Future[Unit] = for {
      _ <- query(Json.obj(
        "statement" ->
          ( "MATCH (book:Skillbook {url: {skillbook}}) "
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
