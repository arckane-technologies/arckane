/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.db

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._

package object tag {

  def createUri (tx: Transaction)(tag: String): Future[String] = for {
    result <- tx execute """MERGE (id:UniqueId {name:'General Entities IDs'})
                            ON CREATE SET id.count = 1
                            ON MATCH SET id.count = id.count + 1
                            RETURN id.count"""
    uri <- result("id.count")(0).as[Int] match {
      case uid => Future("/"+tag.toLowerCase+"/"+uid.toString)
    }
  } yield uri

  //def create (tx: Transaction, props: JsObject): Future[Arcklet[T, JsObject]] = for {
  //  uri <- createUri(tx)
  //  _ <- tx execute Json.obj(
  //    "statement" -> ("CREATE (n:"+name+" {props})"),
  //    "parameters" -> Json.obj(
  //      "props" -> (Json.obj("uri" -> uri) ++ props)
  //    ))
  //} yield Arcklet(tag, uri, props)

  //def create (props: JsObject): Future[Arcklet[T, JsObject]] = for {
  //  tx <- openTransaction
  //  arcklet <- create(tx, props)
  //  _ <- tx.finish
  //} yield arcklet

  //def get (uri: String): Future[Option[Arcklet[T, JsObject]]] = for {
  //  result <- query(Json.obj(
  //    "statement" -> ("MATCH (n:"+name+" {uri: {urimatcher}}) RETURN n"),
  //    "parameters" -> Json.obj(
  //      "urimatcher" -> uri
  //    )))
  //} yield if (result(0)("n").length > 0)
  //    Some(Arcklet(tag, uri, result(0)("n")(0).as[JsObject]))
  //  else
  //    None

  //def getWith[P] (uri: String)(implicit pr: Reads[P]): Future[Option[Arcklet[T, P]]] = for {
  //  result <- query(Json.obj(
  //    "statement" -> ("MATCH (n:"+name+" {uri: {urimatcher}}) RETURN n"),
  //    "parameters" -> Json.obj(
  //      "urimatcher" -> uri
  //    )))
  //} yield if(result(0)("n").length > 0)
  //    Some(Arcklet(tag, uri, result(0)("n")(0).as[P]))
  //  else
  //    None

  //def getProps (uri: String): Future[JsObject] = for {
  //  result <- query(Json.obj(
  //    "statement" -> ("MATCH (n:"+name+" {uri: {urimatcher}}) RETURN n"),
  //    "parameters" -> Json.obj(
  //      "urimatcher" -> uri
  //    )))
  //} yield if (result(0)("n").length > 0)
  //    result(0)("n")(0).as[JsObject]
  //  else
  //    Json.obj("error" -> "not found")

  //def createWith[P] (tx: Transaction, props: P)(implicit pw: Writes[P]): Future[Arcklet[T, P]] = for {
  //  uri <- createUri(tx)
  //  _ <- tx execute Json.obj(
  //    "statement" -> ("CREATE (n:"+name+" {props})"),
  //    "parameters" -> Json.obj(
  //      "props" -> (Json.obj("uri" -> uri) ++ Json.toJson(props).as[JsObject])
  //    ))
  //} yield Arcklet(tag, uri, props)

  //def createWith[P] (props: P)(implicit pw: Writes[P]): Future[Arcklet[T, P]] = for {
  //  tx <- openTransaction
  //  arcklet <- createWith(tx, props)
  //  _ <- tx.finish
  //} yield arcklet

  //def count: Future[Int] = for {
  //  tx <- openTransaction
  //  result <- tx lastly Json.obj(
  //    "statement" -> ("MATCH (n:"+name+") RETURN count(n)"))
  //  count <- Future(result(0)("count(n)")(0).as[Int])
  //} yield count
}
