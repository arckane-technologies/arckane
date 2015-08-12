package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._

package object skillbook {

  trait Skillbook
  object SkillbookTag extends Tag[Skillbook]("Skillbook")
  case class SkillbookInfo (name: String, description: String)

  implicit val skillbookInfoWrites = new Writes[SkillbookInfo] {
    def writes(props: SkillbookInfo) = Json.obj(
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillbookInfoReads = (
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillbookInfo.apply _)

  //def addRootSkill ()
  //def removeRootSkill ()
}
