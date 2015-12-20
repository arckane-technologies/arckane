/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.db

import scala.concurrent.Future

import play.api.libs.ws._
import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._

package object txresponse {

  /** RuntimeException for non-expected http response status. */
  case class StatusErr (message: String) extends RuntimeException(message)

  /** RuntimeException for errors when deserializing json. */
  case class DeserializationErr (message: String) extends RuntimeException(message)

  /** Injects methods to Play Framework's WSResponse type. */
  implicit class WSResponseOps (response: WSResponse) {

    /** Raises a future from a WSResponse. */
    def future: Future[WSResponse] = Future(response)

    /** Throws an [[StatusErr]] if the WSResponse has a different status code than
      * the expected one.
      *
      * @param status expected.
      * @return the same WSResponse in a Future.
      */
    def statusMustBe (status: Int): Future[WSResponse] =
      if (response.status != status)
        throw StatusErr("Status had to be "+status.toString+" in response: " + response.toString)
      else
        response.future

    /** Deserializes the json in the WSResponse by converting it to a data type.
      *
      * @param reads implicit, a Reads Play Framework json converter, normally
      * obtained by importing a package that has declared the implicit reads.
      * @return the converted json as the data type inside a Future.
      */
    def deserialize[A](implicit reads: Reads[A]): Future[A] = Future(response.json.as[A])

    def checkForTransactionErrs: Future[WSResponse] =
      (response.json \ "errors").as[Seq[DeserializedTxErr]] match {
        case errs =>
          if (errs.length > 0)
            throw TxErr(errs.toString)
          else
            response.future
      }

    /** Creates a [[Transaction]] data type out of the json of the WSResponse. */
    def transaction: Future[Transaction] = Future(response.json.as[Transaction] match {
      case tx => Transaction(tx.commit.reverse.drop(7).reverse, tx.commit, tx.expires)
    })

    /** Creates a list of [[TxResult]] data type out of the response json. Check the
      * cypher transactional endpoint documentation to see the structure of the result.
      */
    def allResults: Future[List[TxResult]] = {
      val results = (response.json \ "results").as[List[JsValue]]
      if (results.length > 0) {
        Future(results.map(deserializeOneResult))
      }
      else {
        Future(List.empty[Map[String, List[JsValue]]])
      }
    }

    /** Creates a [[TxResult]] data type out of the response json. Check the
      * cypher transactional endpoint documentation to see the structure of the result.
      */
    def result: Future[TxResult] = {
      val results = (response.json \ "results").as[List[JsValue]]
      if (results.length > 0) {
        Future(deserializeOneResult(results.head))
      }
      else {
        Future(Map.empty[String, List[JsValue]])
      }
    }

    /** Check the cypher transactional endpoint documentation to see the structure of the result. */
    private def deserializeOneResult (json: JsValue): TxResult = {
      val columns = (json \ "columns").as[List[String]] zip Stream.from(0)
      val data = (json \ "data" \\ "row").map(_.as[List[JsValue]])
      (columns.map { case (col, i) =>
        (col -> (data.map { json =>
          json(i).as[JsValue]
        }).toList)
      }).toMap
    }
  }
}
