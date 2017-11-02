package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class SkillBonus(
    skill: String,
    @ApiModelProperty(required = false, allowableValues = "is,starts with,contains")
    skillCompare: String = NameCompare.IS,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(
      required = false,
      allowableValues = "is,starts with,contains,is not",
      allowEmptyValue = true)
    spcCompare: Option[String] = None,
    @ApiModelProperty(required = false)
    perLvl: Boolean = false,
    bonus: Int = 0) extends SkillBased

object SkillBonus {
  private  val skillCompareErr            =
    JsonValidationError(s"Skill comparison type can only be one of: ${NameCompare.canBe mkString ", "}")
  private  val spcCompareErr              =
    JsonValidationError(s"Specialization comparison type can only be one of: ${NameCompare.spcCanBe mkString ", "}")
  private  val reads : Reads[SkillBonus]  = (
    (JsPath \ NameOf.nameOf[SkillBonus](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[SkillBonus](_.skillCompare))
        .readWithDefault(NameCompare.IS)
        .filter(skillCompareErr)(NameCompare.canBe) ~
      (JsPath \ NameOf.nameOf[SkillBonus](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[SkillBonus](_.spcCompare))
        .readNullable[String]
        .filter(spcCompareErr)(_ forall NameCompare.spcCanBe) ~
      (JsPath \ NameOf.nameOf[SkillBonus](_.perLvl)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[SkillBonus](_.bonus)).read[Int]) (SkillBonus.apply _)
  implicit val format: Format[SkillBonus] = Format(reads, Json.writes[SkillBonus])
}
