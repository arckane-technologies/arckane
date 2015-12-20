/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.db

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.transaction._

object Node {

  def get (uri: String): Future[Option[JsObject]] = for {
    result <- query(Json.obj(
      "statement" -> ("MATCH (n {uri: {urimatcher}}) RETURN n"),
      "parameters" -> Json.obj(
        "urimatcher" -> uri
      )))
  } yield result("n").length match {
    case 0 => None
    case _ => Some(result("n").head.as[JsObject])
  }

  def create (props: JsObject)(tag: String): Future[String] = for {
    tx <- openTransaction
    uri <- create(tx, props)(tag)
    _ <- tx.finish
  } yield uri

  def create (tx: Transaction, props: JsObject)(tag: String): Future[String] = for {
    uri <- createUri(tx)(tag)
    _ <- tx execute Json.obj(
      "statement" -> ("CREATE (n:"+tag+" {props})"),
      "parameters" -> Json.obj(
        "props" -> (Json.obj("uri" -> uri) ++ props)
      ))
  } yield uri

  def createUri (tx: Transaction)(tag: String): Future[String] = for {
    result <- tx execute """MERGE (id:UniqueId {name:'General Entities IDs'})
                            ON CREATE SET id.count = 1
                            ON MATCH SET id.count = id.count + 1
                            RETURN id.count"""
  } yield result("id.count").head.as[Int].toString

  def count (tag: String): Future[Int] = for {
    result <- query("MATCH (n:"+tag+") RETURN count(n)")
  } yield result("count(n)").head.as[Int]

  def set (uri: String, props: JsObject): Future[Unit] = for {
    _ <- query(Json.obj(
      "statement" -> ("MATCH (n {uri: {urimatcher}}) SET n += {props}"),
      "parameters" -> Json.obj(
        "urimatcher" -> uri,
        "props" -> props
      )))
  } yield Unit

  def delete (uri: String): Future[Unit] = for {
    _ <- query(Json.obj(
      "statement" -> ("MATCH (n {uri: {urimatcher}}) OPTIONAL MATCH (n)-[r]-() DELETE n,r"),
      "parameters" -> Json.obj(
        "urimatcher" -> uri
      )))
  } yield Unit
}
