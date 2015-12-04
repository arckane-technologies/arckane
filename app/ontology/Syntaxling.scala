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
import ontology.wikimedia._

/** Data types and type classes of Syntaxling. */
package object syntaxling {

  /** Syntaxling type. */
  trait Syntaxling

  /** Data type for Neo4j :Syntaxling tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SyntaxlingTag extends Tag[Syntaxling]("Syntaxling")

  /** Type class for the [[SyntaxlingTag]] data type. */
  implicit class SyntaxlingTagOps (tag: Tag[Syntaxling]) {

    /** Searches first in the database for matching syntaxlings, if none matches
      * searches in wikipedia.
      *
      * @param queryString of the syntaxling is being searched.
      * @return an array of json objects with the syntaxling info.
      */
    def search (queryString: String): Future[JsValue] = for {
      result <- query(Json.obj(
        "statement" ->
          ( "MATCH (n:"+tag.str+") WHERE n.title =~ { regex } "
          + "RETURN n.title, n.web "
          + "LIMIT 5"),
        "parameters" -> Json.obj(
          "regex" -> ("(?i).*"+queryString.trim.escapeParenthesis+".*")
        )))
      response <- if (result(0)("n.title").length > 0) {
        Future(Json.toJson(result(0)("n.title").zipWithIndex.map { case (title, index) => Json.obj(
          "from_database" -> true,
          "title" -> title,
          "web" -> result(0)("n.web")(index)
        )}).as[JsArray])
      } else {
        wikiSearchPrefix(queryString.trim)
      }
    } yield response

  }
}
