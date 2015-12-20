/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.ontology

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._
import arckane.db.utils._
import arckane.ontology.wikimedia._

/** Data types and type classes of Syntaxling. */
package object syntaxling {

  /** Searches first in the database for matching syntaxlings, if none matches
    * searches in wikipedia.
    *
    * @param queryString of the syntaxling is being searched.
    * @return an array of json objects with the syntaxling info.
    */
  def syntaxlingSearch (queryString: String): Future[JsValue] = for {
    result <- query(Json.obj(
      "statement" ->
        ( "MATCH (n:Topic) WHERE n.title =~ { regex } "
        + "RETURN n.title, n.web "
        + "LIMIT 5"),
      "parameters" -> Json.obj(
        "regex" -> ("(?i).*"+queryString.trim.escapeParenthesis+".*")
      )))
    response <- if (result("n.title").length > 0) {
      Future(Json.toJson(result("n.title").zipWithIndex.map { case (title, index) => Json.obj(
        "from_database" -> true,
        "title" -> title,
        "web" -> result("n.web")(index)
      )}).as[JsArray])
    } else {
      wikiSearchPrefix(queryString.trim)
    }
  } yield response
}
