/**
  * @author Francisco Miguel Aramburo Torres - atfm05@gmail.com
  */

package ontology

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.neo4j._
import database.persistence._

/** Data types and type classes of Syntaxling. */
package object syntaxling {

  /** Syntaxling type. */
  trait Syntaxling

  /** Data type for Neo4j :Syntaxling tag. Specializes the more general type [[database.persistence.Tag]]. */
  object SyntaxlingTag extends Tag[Syntaxling]("Syntaxling")

  /** Type class for the [[SyntaxlingTag]] data type. */
  implicit class SkillTagOps (tag: Tag[Syntaxling]) {

  }
}
