/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package ontology

import scala.concurrent.Future

import play.api._
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.libs.json._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._
import play.api.libs.functional.syntax._

package object wikimedia {

  def wikiSearchPrefix (prefix: String): Future[JsValue] = for {
    response <- WS.url("https://en.wikipedia.org/w/api.php")
      .withHeaders("Api-User-Agent" -> "Arckane/0.1")
      .withQueryString(
        "action" -> "query",
        "list" -> "prefixsearch",
        "pssearch" -> prefix,
        "format" -> "json")
      .get()
  } yield {println(response.json); response.json}
}
