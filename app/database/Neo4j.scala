/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package arckane.db

import scala.concurrent.Future

import play.api._
import play.api.libs.ws._
import play.api.libs.ws.ning._
import play.api.Play.current
import play.api.libs.concurrent.Execution.Implicits._

import arckane.db.txresponse._

/** Functions, data types and type classes to interact easily with Neo4j. The
  * interaction is made using Neo4j's RESTful cypher transactional
  * endpoint.
  *
  * @see [[http://neo4j.com/docs/stable/rest-api.html]] documentation for Neo4j's REST API.
  */
package object neo4j {

  /** Neo4j addess taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_ADDRESS environment variable in the system. */
  val address = Play.current.configuration.getString("neo.address").get

  /** Neo4j user taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_USER environment variable in the system. */
  val user = Play.current.configuration.getString("neo.user").get

  /** Neo4j password taken from the application.conf file. If no confing is found
    * play framework looks for the NEO4J_PASSWORD environment variable in the system. */
  val password = Play.current.configuration.getString("neo.password").get

  /** Creates a Play Framework's WSRequest with added authentication.
    *
    * @param path url to where the request is going to be made.
    * @return a WSRequest object with authentication added, taken from the user
    * and password values.
    */
  def withAuth (path: String): WSRequest =
    WS.url(path).withAuth(user, password, WSAuthScheme.BASIC)

  /** Sends a request HEAD to the Neo4j server to check for reachability and checks
    * that the response status is 200.
    *
    * @return a WSResponse result from the HEAD request.
    */
  def reachable: Future[WSResponse] = for {
    response <- withAuth(address).head()
    _ <- response statusMustBe 200
  } yield response
}
