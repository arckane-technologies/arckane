package decision

import scala.concurrent.Future

import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.libs.concurrent.Execution.Implicits._

import database.persistence._

package object skill {

  trait Skill
  object SkillTag extends Tag[Skill]("Skill")
  case class SkillInfo (name: String, description: String)

  implicit val skillInfoWrites = new Writes[SkillInfo] {
    def writes(props: SkillInfo) = Json.obj(
      "name" -> props.name,
      "description" -> props.description
    )
  }

  implicit val skillInfoReads = (
    (JsPath \ "name").read[String] and
    (JsPath \ "description").read[String]
  )(SkillInfo.apply _)


  //def requires (source: Skill, target: Skill, relevance: Int): Future[Validation[Err, Relationship]] = for {
  //  relationship <- createRelationship(source.node, target.node, "REQUIRES", Json.obj("relevance" -> relevance))
  //} yield relationship

  //def setRequisiteValue (source: Skill, target: Skill)
}
