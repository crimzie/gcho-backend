package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import models.charlist.skill.SkillBaseAttribute
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class AttributeCostBonus(attr: String = SkillBaseAttribute.ST, cost: Int = 0)

object AttributeCostBonus {
  private  val attrErr                            =
    JsonValidationError(s"Attribute can only be one of: ${SkillBaseAttribute.canBe mkString ", "}")
  private  val reads : Reads[AttributeCostBonus]  = (
    (JsPath \ NameOf.nameOf[AttributeCostBonus](_.attr)).read[String].filter(attrErr)(SkillBaseAttribute.canBe) ~
      (JsPath \ NameOf.nameOf[AttributeCostBonus](_.cost)).read[Int]) (AttributeCostBonus.apply _)
  implicit val format: Format[AttributeCostBonus] = Format(reads, Json.writes[AttributeCostBonus])
}
