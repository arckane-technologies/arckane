package decision

import play.api.libs.json._
import play.api.libs.functional.syntax._

import utils.DatabaseOps.Node
import utils.Persistent
import utils.Entity

object SkillbookOps {

  case class Skillbook (id: Int, node: Node, props: SkillbookProps) extends Entity
  case class SkillbookProps (url: String, name: String, description: String)

  implicit object SkillbookPersistent extends Persistent[Skillbook, SkillbookProps] {
    val tag: String = "Skillbook"
    val entityWrites: Writes[SkillbookProps] = new Writes[SkillbookProps] {
      def writes(props: SkillbookProps) = Json.obj(
        "url" -> props.url,
        "name" -> props.name,
        "description" -> props.description
      )
    }
    val entityReads: Reads[SkillbookProps] = (
      (JsPath \ "url").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String]
    )(SkillbookProps.apply _)
    def instantiate (props: SkillbookProps, node: Node) = Skillbook(node.id, node, props)
  }

  private val db = SkillbookPersistent

  //def addRootSkill ()
  //def removeRootSkill ()
}
