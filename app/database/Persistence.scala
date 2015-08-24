package database

import scala.concurrent._

import play.api.libs.json._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._

package object persistence {

  case class Tag[T](str: String)

  case class Arcklet[T, P] (tag: Tag[T], url: String, props: P)

  implicit class TagOps[T] (tag: Tag[T]) {

    def get (url: String): Future[Arcklet[T, JsObject]] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {url: {urlmatcher}}) RETURN n"),
        "parameters" -> Json.obj(
          "urlmatcher" -> url
        ))
      props <- Future(result(0)("n")(0).as[JsObject])
    } yield Arcklet(tag, url, props)

    def getWith[P] (url: String)(implicit pr: Reads[P]): Future[Arcklet[T, P]] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {url: {urlmatcher}}) RETURN n"),
        "parameters" -> Json.obj(
          "urlmatcher" -> url
        ))
      props <- Future(result(0)("n")(0).as[P])
    } yield Arcklet(tag, url, props)

    def create (tx: Transaction, props: JsObject): Future[Arcklet[T, JsObject]] = for {
      url <- createUrl(tx)
      _ <- tx execute Json.obj(
        "statement" -> ("CREATE (n:"+tag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj("url" -> url) ++ props)
        ))
    } yield Arcklet(tag, url, props)

    def create (props: JsObject): Future[Arcklet[T, JsObject]] = for {
      tx <- openTransaction
      arcklet <- create(tx, props)
      _ <- tx.finish
    } yield arcklet

    def createWith[P] (tx: Transaction, props: P)(implicit pw: Writes[P]): Future[Arcklet[T, P]] = for {
      url <- createUrl(tx)
      _ <- tx execute Json.obj(
        "statement" -> ("CREATE (n:"+tag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj("url" -> url) ++ Json.toJson(props).as[JsObject])
        ))
    } yield Arcklet(tag, url, props)

    def createWith[P] (props: P)(implicit pw: Writes[P]): Future[Arcklet[T, P]] = for {
      tx <- openTransaction
      arcklet <- createWith(tx, props)
      _ <- tx.finish
    } yield arcklet

    def count: Future[Int] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+") RETURN count(n)"))
      count <- Future(result(0)("count(n)")(0).as[Int])
    } yield count

    def createUrl (tx: Transaction): Future[String] = for {
      result <- tx execute """MERGE (id:UniqueId{name:'General Entities IDs'})
                              ON CREATE SET id.count = 1
                              ON MATCH SET id.count = id.count + 1
                              RETURN id.count"""
      url <- result(0)("id.count")(0).as[Int] match {
        case uid => Future("/"+tag.str.toLowerCase+"/"+uid.toString)
      }
    } yield url
  }

  implicit class ArckletOps[T, P] (arcklet: Arcklet[T, P]) {

    def get[A] (prop: String)(implicit pr: Reads[A]): Future[A] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (n:${arcklet.tag.str} {url: {urlmatcher}}) RETURN n.$prop"""),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url,
          "prop" -> prop))
    } yield result(0)("n."+prop)(0).as[A]

    def set[A] (prop: String, value: A)(implicit pw: Writes[A]): Future[Arcklet[T, (String, A)]] = for {
      tx <- openTransaction
      _ <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (n:${arcklet.tag.str} {url: {urlmatcher}}) SET n.$prop = {value}"""),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url,
          "prop" -> prop,
          "value" -> Json.toJson(value)))
    } yield Arcklet(arcklet.tag, arcklet.url, prop -> value)

    def set (props: JsObject): Future[Arcklet[T, JsObject]] = for {
      tx <- openTransaction
      _ <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (n:${arcklet.tag.str} {url: {urlmatcher}}) SET n += {props}"""),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url,
          "props" -> props))
    } yield Arcklet(arcklet.tag, arcklet.url, props)

    def relate[A, B] (that: Arcklet[A, B], tx: Transaction, relType: String): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> (s"""MATCH (a:${arcklet.tag.str}),(b:${that.tag.str})
                            WHERE a.url = {aurl} AND b.url = {burl}
                            CREATE (a)-[r:${relType}]->(b)"""),
        "parameters" -> Json.obj(
          "aurl" -> arcklet.url,
          "burl" -> that.url))
    } yield Unit

    def relate[A, B] (that: Arcklet[A, B], tx: Transaction, relType: String, props: JsObject): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> (s"""MATCH (a:${arcklet.tag.str}),(b:${that.tag.str})
                            WHERE a.url = {aurl} AND b.url = {burl}
                            CREATE (a)-[r:${relType} {props}]->(b)"""),
        "parameters" -> Json.obj(
          "aurl" -> arcklet.url,
          "burl" -> that.url,
          "props" -> props))
    } yield Unit

    def relate[A, B] (that: Arcklet[A, B], relType: String): Future[Unit] = for {
      tx <- openTransaction
      _ <- arcklet relate(that, tx, relType)
      _ <- tx.finish
    } yield Unit

    def relate[A, B] (that: Arcklet[A, B], relType: String, props: JsObject): Future[Unit] = for {
      tx <- openTransaction
      _ <- arcklet relate(that, tx, relType, props)
      _ <- tx.finish
    } yield Unit

    def deleteRelationships (tx: Transaction): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> ("MATCH (n:"+arcklet.tag.str+" {url: {urlmatcher}})-[r]-() DELETE r"),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url))
    } yield Unit

    def deleteNode (tx: Transaction): Future[Unit] = for {
      _ <- tx execute Json.obj(
        "statement" -> ("MATCH (n:"+arcklet.tag.str+" {url: {urlmatcher}}) DELETE n"),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url))
    } yield Unit

    def delete: Future[Unit] = for {
      tx <- openTransaction
      _ <- arcklet deleteRelationships tx
      _ <- arcklet deleteNode tx
      _ <- tx.finish
    } yield Unit

    def writes (implicit wr: Writes[P]): Writes[Arcklet[T, P]] = new Writes[Arcklet[T, P]] {
      def writes(arcklet: Arcklet[T, P]) = Json.obj(
        "type" -> arcklet.tag.str,
        "url" -> arcklet.url
      ) ++ Json.toJson(arcklet.props).as[JsObject]
    }
  }
}
