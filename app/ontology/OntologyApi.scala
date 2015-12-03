/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package ontology

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._
import ontology.syntaxling._

/** Play Framework controller for the ontology service. */
class OntologyApi extends Controller {

  /** Something...
    * Route: GET /api/ontology/search
    * Sessions variables: home
    * Query string variables: search
    */
  def search = Action.async { request =>
    (for {
      //user <- request.session.get("home")
      search <- request.queryString.get("search")
      response <- Some(SyntaxlingTag.search(search.head))
    } yield response.map { array =>
      Ok(array)
    }).getOrElse {
      Future(BadRequest("Expected 'search' query string."))
    }
  }

}
