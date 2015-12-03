/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package ontology

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of Syntaxling. */
package object syntaxling {

  /** Syntaxling type. */
  trait Syntaxling

  /** Data type for Neo4j :Syntaxling tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SyntaxlingTag extends Tag[Syntaxling]("Syntaxling")

  /** Type class for the [[SyntaxlingTag]] data type. */
  implicit class SkillTagOps (tag: Tag[Syntaxling]) {

    def search (queryString: String): Future[JsArray] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (n:"+tag.str+") WHERE n.name =~ { regex } "
          + "RETURN n.name, n.url, n.description "
          + "LIMIT 5"),
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+queryString.trim.escapeParenthesis+".*")
        )))
    } yield if (result.length > 0) {
      Json.toJson(result(0)("n.name").zipWithIndex.map { case (name, index) => Json.obj(
        "name" -> name,
        "url" -> result(0)("n.url")(index)
      )}).as[JsArray]
    } else {
      Json.arr(42)
    }

  }
}
