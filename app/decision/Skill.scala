package decision

import play.api.libs.json._
import play.api.libs.functional.syntax._

import utils.DatabaseOps.Node
import utils.Persistent
import utils.Entity

object SkillOps {

  case class Skill (id: Int, node: Node, props: SkillProps) extends Entity
  case class SkillProps (url: String, name: String, description: String)

  implicit object SkillPersistent extends Persistent[Skill, SkillProps] {
    val tag: String = "Skill"
    val entityWrites: Writes[SkillProps] = new Writes[SkillProps] {
      def writes(props: SkillProps) = Json.obj(
        "url" -> props.url,
        "name" -> props.name,
        "description" -> props.description
      )
    }
    val entityReads: Reads[SkillProps] = (
      (JsPath \ "url").read[String] and
      (JsPath \ "name").read[String] and
      (JsPath \ "description").read[String]
    )(SkillProps.apply _)
    def instantiate (props: SkillProps, node: Node) = Skill(node.id, node, props)
  }

  private val db = SkillPersistent

  //def requires (source: Skill, target: Skill)
  //def setRequisiteValue (source: Skill, target: Skill)
}
