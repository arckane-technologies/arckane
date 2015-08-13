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
        )
      )
      props <- Future((result.head("n") \ "row")(0).as[JsObject])
    } yield Arcklet(tag, url, props)

    def getWith[P] (url: String)(implicit pr: Reads[P]): Future[Arcklet[T, P]] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+" {url: {urlmatcher}}) RETURN n"),
        "parameters" -> Json.obj(
          "urlmatcher" -> url
        )
      )
      props <- result.get[P]
    } yield Arcklet(tag, url, props)

    def create (props: JsObject): Future[Arcklet[T, JsObject]] = for {
      tx <- openTransaction
      url <- tx createUrl tag
      result <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+tag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj("url" -> url) ++ props)
        )
      )
    } yield Arcklet(tag, url, props)

    def createWith[P] (props: P)(implicit pw: Writes[P]): Future[Arcklet[T, P]] = for {
      tx <- openTransaction
      url <- tx createUrl tag
      result <- tx lastly Json.obj(
        "statement" -> ("CREATE (n:"+tag.str+" {props})"),
        "parameters" -> Json.obj(
          "props" -> (Json.obj("url" -> url) ++ Json.toJson(props).as[JsObject])
        )
      )
    } yield Arcklet(tag, url, props)

    def count: Future[Int] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> ("MATCH (n:"+tag.str+") RETURN count(n)"))
      count <- proceed((result.head("count(n)") \ "row")(0).validate[Int])(identity)
    } yield count
  }

  implicit class ArckletOps[T, P] (arcklet: Arcklet[T, P]) {

    def set[A] (prop: String, value: A)(implicit pw: Writes[A]): Future[Arcklet[T, (String, A)]] = for {
      tx <- openTransaction
      _ <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (n:${arcklet.tag.str} {url: {urlmatcher}})
                            SET n.${prop} = {value}"""),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url,
          "prop" -> prop,
          "value" -> Json.toJson(value)
        )
      )
    } yield Arcklet(arcklet.tag, arcklet.url, prop -> value)

    def relate[A, B] (that: Arcklet[A, B], relType: String): Future[Unit] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (a:${arcklet.tag.str}),(b:${that.tag.str})
                            WHERE a.url = {aurl} AND b.url = {burl}
                            CREATE (a)-[r:${relType}]->(b)"""),
        "parameters" -> Json.obj(
          "aurl" -> arcklet.url,
          "burl" -> that.url
        )
      )
    } yield Unit

    def relate[A, B] (that: Arcklet[A, B], relType: String, props: JsObject): Future[Unit] = for {
      tx <- openTransaction
      result <- tx lastly Json.obj(
        "statement" -> (s"""MATCH (a:${arcklet.tag.str}),(b:${that.tag.str})
                            WHERE a.url = {aurl} AND b.url = {burl}
                            CREATE (a)-[r:${relType} {props}]->(b)"""),
        "parameters" -> Json.obj(
          "aurl" -> arcklet.url,
          "burl" -> that.url,
          "props" -> props
        )
      )
    } yield Unit

    def deleteRelationships (tx: Transaction): Future[Arcklet[T, P]] = for {
      _ <- tx execute Json.obj(
        "statement" -> ("MATCH (n:"+arcklet.tag.str+" {url: {urlmatcher}})-[r]-() DELETE r"),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url
        )
      )
    } yield arcklet

    def deleteNode (tx: Transaction): Future[Arcklet[T, P]] = for {
      _ <- tx execute Json.obj(
        "statement" -> ("MATCH (n:"+arcklet.tag.str+" {url: {urlmatcher}}) DELETE n"),
        "parameters" -> Json.obj(
          "urlmatcher" -> arcklet.url
        )
      )
    } yield arcklet

    def delete: Future[Arcklet[T, P]] = for {
      tx <- openTransaction
      _ <- arcklet deleteRelationships tx
      _ <- arcklet deleteNode tx
      _ <- tx.finish
    } yield arcklet
  }

  implicit class PersistenceTxOps (tx: Transaction) {

    def createUrl[T] (tag: Tag[T]): Future[String] = for {
      result <- tx execute """MERGE (id:UniqueId{name:'General Entities IDs'})
                              ON CREATE SET id.count = 1
                              ON MATCH SET id.count = id.count + 1
                              RETURN id.count"""
      url <- proceed((result.head("id.count") \ "row")(0).validate[Int]) {
        case uid => "/"+tag.str.toLowerCase+"/"+uid.toString
      }
    } yield url
  }

  implicit class PersistenceTxResultOps (txr: TxResult) {

    def get[P](implicit reads: Reads[P]): Future[P] =
      proceed((txr.head("n") \ "row")(0).validate[P])(identity)
  }
}
