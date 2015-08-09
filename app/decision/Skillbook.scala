package decision

import scala.concurrent.Future

import scalaz.{Validation, Success, Failure}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.Persistent
import utils.Entity
import utils.ValidationOps._

object SkillbookOps {

  case class Skillbook (id: Int, node: Node, props: SkillbookProps) extends Entity
  case class SkillbookProps (url: String, name: String, description: String)

  implicit val skillbookPropsWrites = new Writes[SkillbookProps] {
    def writes(props: SkillbookProps) = Json.obj(
      "url" -> props.url,
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillbookWrites = new Writes[Skillbook] {
    def writes(props: Skillbook) = Json.obj(
      "id" -> props.id,
      "node" -> props.node,
      "props" -> props.props
    )
  }

  implicit val skillbookPropsReads = (
    (JsPath \ "url").read[String] and
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillbookProps.apply _)

  implicit val skillbookReads = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "node").read[Node] and
    (JsPath \ "props").read[SkillbookProps]
  )(Skillbook.apply _)

  implicit object SkillbookPersistent extends Persistent[Skillbook, SkillbookProps] {
    val tag: String = "Skillbook"
    val entityWrites: Writes[SkillbookProps] = skillbookPropsWrites
    val entityReads: Reads[SkillbookProps] = skillbookPropsReads
    def instantiate (props: SkillbookProps, node: Node) = Skillbook(node.id, node, props)
  }

  private val db = SkillbookPersistent

  //def addRootSkill ()
  //def removeRootSkill ()
}
