/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.ontology

import scala.concurrent._

import play.api._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._
import arckane.ontology.syntaxling._

/** Play Framework controller for the ontology service. */
class OntologyApi extends Controller {

  /** Searches for syntaxlings (right now only wikipedia articles) which names
    * are similar to an input.
    * Route: GET /api/ontology/search
    * Sessions variables: home
    * Query string variables: search
    */
  def search = Action.async { request =>
    (for {
      user <- request.session.get("home")
      search <- request.queryString.get("search")
      response <- Some(syntaxlingSearch(search.head))
    } yield response.map { array =>
      Ok(array)
    }).getOrElse {
      Future(BadRequest("Expected 'search' query string."))
    }
  }

}
