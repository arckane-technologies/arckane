package utils

import scala.concurrent.Future

import play.api._
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

object Database {

  val address = Play.current.configuration.getString("neo4j.address").get

  val user = Play.current.configuration.getString("neo4j.user").get

  val password = Play.current.configuration.getString("neo4j.password").get

  val queryPath = "http://" + address + "/db/data"

  def withAuth (url: String): WSRequest =
    WS.url(url).withAuth(user, password, WSAuthScheme.BASIC)

  def ping: Future[Boolean] = for {
    response <- withAuth(queryPath).head()
  } yield response.status == 200
}
