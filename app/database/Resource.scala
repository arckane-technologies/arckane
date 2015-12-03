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

/** @deprecated(No longer needed for the distributed education system, v0.1.1)
  * Data types and type classes of Resource.
  */
package object resource {

  /** Resource type. */
  trait Resource

  /** Data type for Neo4j :Resource tag. Specializes the more general type [[database.persistence.Tag]]. */
  object ResourceTag extends Tag[Resource]("Resource")

  /** Type class for the [[ResourceTag]] data type. */
  implicit class ResourceTagOps (tag: Tag[Resource]) {

    /** Creates an external resource related to a skill.
      *
      * @param rtype relationship type, (see arckane-polymer project <resource-types>
      * element for more info about resource types.)
      * @param forSkill skill url-id to be related with this resource.
      * @param name of the resource.
      * @param url locator of the external resource.
      * @param description of the resource.
      * @param user url-id of the user that is proposing the resource.
      * @return json object with the url-id of the created resource. {url: string}
      */
    def propose (rtype: String, forSkill: String, name: String, url: String, description: String, user: String): Future[JsObject] = for {
      tx <- openTransaction
      arcklet <- ResourceTag.create(tx, Json.obj(
        "name" -> name.trim.clean.capitalizeWords,
        "resourceType" -> rtype,
        "resourceUrl" -> url.trim,
        "description" -> description.trim,
        "infusionValue" -> 0
      ))
      result <- tx.lastly(Json.obj(
        "statement" ->
          ( "MATCH (a:Skill {name: {source}}),(b:Resource {url: {target}}),(u:User {url: {user}}) "
          + "MERGE (a)-[:HAS_RESOURCE]->(b)<-[:PROPOSES]-(u) "
          + "RETURN a.url"),
        "parameters" -> Json.obj(
          "source" -> forSkill,
          "target" -> arcklet.url,
          "user" -> user
        )
      ))
    } yield Json.obj("url" -> result(0)("a.url")(0))

    /** Creates a directed relationship between two skills.
      *
      * @param forSkill (source) skill url-id which is related to the receiving skill.
      * @param relatedSkill (target) skill url-id which receives the relationship.
      * @param user url-id of the user that is proposing the relationship.
      * @return json object with the url-id of the source. {url: string}
      */
    def proposeSkillRelationship (forSkill: String, relatedSkill: String, user: String): Future[JsObject] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (a:Skill {name: {source}}),(b:Skill {name: {target}}) "
          + "MERGE (a)-[r:RELATED]->(b) "
          + "ON CREATE SET r.infusionValue = 0 "
          + "RETURN a.url"),
        "parameters" -> Json.obj(
          "source" -> forSkill.trim,
          "target" -> relatedSkill.trim
        )
      ))
    } yield Json.obj("url" -> result(0)("a.url")(0))
  }
}
