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

object SkillOps {

  case class Skill (id: Int, node: Node, props: SkillProps) extends Entity
  case class SkillProps (url: String, name: String, description: String)

  implicit val skillPropsWrites = new Writes[SkillProps] {
    def writes(props: SkillProps) = Json.obj(
      "url" -> props.url,
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillWrites = new Writes[Skill] {
    def writes(skill: Skill) = Json.obj(
      "id" -> skill.id,
      "node" -> skill.node,
      "props" -> skill.props
    )
  }

  implicit val skillPropsReads = (
    (JsPath \ "url").read[String] and
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillProps.apply _)

  implicit val skillReads = (
    (JsPath \ "id").read[Int] and
    (JsPath \ "node").read[Node] and
    (JsPath \ "props").read[SkillProps]
  )(Skill.apply _)

  implicit object SkillPersistent extends Persistent[Skill, SkillProps] {
    val tag: String = "Skill"
    val entityWrites: Writes[SkillProps] = skillPropsWrites
    val entityReads: Reads[SkillProps] = skillPropsReads
    def instantiate (props: SkillProps, node: Node) = Skill(node.id, node, props)
  }

  private val db = SkillPersistent

  def requires (source: Skill, target: Skill, relevance: Int): Future[Validation[Err, Relationship]] = for {
    relationship <- createRelationship(source.node, target.node, "REQUIRES", Json.obj("relevance" -> relevance))
  } yield relationship

  //def setRequisiteValue (source: Skill, target: Skill)
}
