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

/** All interaction with the wikipedi api. */
package object wikimedia {

  /** Uses the wikimedia api to search in the english wikipedia for title given a prefix.
    *
    * @param prefix to be searched on.
    * @return an array of json objects with the articles info.
    */
  def wikiSearchPrefix (prefix: String): Future[JsValue] = for {
    response <- WS.url("https://en.wikipedia.org/w/api.php")
      .withHeaders("Api-User-Agent" -> "Arckane/0.1")
      .withQueryString(
        "action" -> "query",
        "list" -> "prefixsearch",
        "pssearch" -> prefix,
        "format" -> "json")
      .get()
    json <- Future(formatResponse(response.json))
  } yield {println(json); json}

  /** Changes the json format from the wikimedia api to a more suitable format for
    * the frontend. Wikipedias article id can be used to access the article using the
    * next url format:
    * https://en.wikipedia.org/?curid=18630637
    *
    * @param json to be formated.
    * @return json formated.
    */
  private def formatResponse (json: JsValue): JsValue = {
    JsArray((json \ "query" \ "prefixsearch") match {
      case JsDefined(arr: JsArray) => arr.value.map { n =>
        Json.obj(
          "from_database" -> false,
          "title" -> ((n \ "title").as[String]),
          "web" -> ("https://en.wikipedia.org/?curid="+((n \ "pageid").as[Int]).toString)
        )
      }
      case _ => Seq[JsValue]()
    })
  }
}
