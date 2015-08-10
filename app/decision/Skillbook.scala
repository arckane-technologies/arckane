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

object SkillbookOps {

  case class Skillbook (props: EntityProps, node: Node) extends Entity

  implicit val skillbookTag = new Tagged[Skillbook] {
    val tag = "Skillbook"
  }

  case class SkillbookBasicInfo (url: String, name: String, description: String) extends EntityProps

  implicit val skillbookBasicInfoWrites = new Writes[SkillbookBasicInfo] {
    def writes(props: SkillbookBasicInfo) = Json.obj(
      "url" -> props.url,
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillbookBasicInfoReads = (
    (JsPath \ "url").read[String] and
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillbookBasicInfo.apply _)

  implicit object SkillbookPersistent extends Persistent[Skillbook, SkillbookBasicInfo] {
    def instantiate (props: SkillbookBasicInfo, node: Node) = Skillbook(props, node)
  }

  //def addRootSkill ()
  //def removeRootSkill ()
}
