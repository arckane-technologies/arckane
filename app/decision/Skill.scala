package decision

import scala.concurrent.Future

import scalaz.{Validation, Success, Failure}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import utils.DatabaseOps._
import utils.ValidationOps._
import utils.Tagged
import utils.Persistent
import utils.{Entity, EntityProps}

object SkillOps {

  case class Skill (props: EntityProps, url: String, node: Node) extends Entity

  implicit val skillTag = new Tagged[Skill] {
    val tag = "Skill"
  }

  case class SkillBasicInfo (name: String, description: String) extends EntityProps

  implicit val SkillBasicInfoWrites = new Writes[SkillBasicInfo] {
    def writes(props: SkillBasicInfo) = Json.obj(
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillBasicInfoReads = (
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillBasicInfo.apply _)

  implicit object SkillPersistent extends Persistent[Skill, SkillBasicInfo] {
    def instantiate (props: SkillBasicInfo, url: String, node: Node) = Skill(props, url, node)
  }

  def requires (source: Skill, target: Skill, relevance: Int): Future[Validation[Err, Relationship]] = for {
    relationship <- createRelationship(source.node, target.node, "REQUIRES", Json.obj("relevance" -> relevance))
  } yield relationship

  //def setRequisiteValue (source: Skill, target: Skill)
}
