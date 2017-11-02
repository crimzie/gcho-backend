package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class DamageBonus(
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
    @ApiModelProperty(dataType = "Int", required = false, allowEmptyValue = true)
    minRelSkill: Option[Int] = None,
    @ApiModelProperty(
      dataType = "Int",
      required = false,
      allowableValues = "range[0, infinity]",
      allowEmptyValue = true)
    perDie: Option[Int] = None,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    minBonus: Int = 0) extends SkillBased

object DamageBonus {
  private  val skillCompareErr             =
    JsonValidationError(s"Skill comparison type can only be one of: ${NameCompare.canBe mkString ", "}")
  private  val spcCompareErr               =
    JsonValidationError(s"Specialization comparison type can only be one of: ${NameCompare.spcCanBe mkString ", "}")
  private  val reads : Reads[DamageBonus]  = (
    (JsPath \ NameOf.nameOf[DamageBonus](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.skillCompare))
        .readWithDefault(NameCompare.IS).filter(skillCompareErr)(NameCompare.canBe) ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.spcCompare))
        .readNullableWithDefault(Some(NameCompare.IS)).filter(spcCompareErr)(_ forall NameCompare.spcCanBe) ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.minRelSkill)).readNullable[Int] ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.perDie)).readNullable(Reads min 0) ~
      (JsPath \ NameOf.nameOf[DamageBonus](_.minBonus)).read(Reads min 0)) (DamageBonus.apply _)
  implicit val format: Format[DamageBonus] = Format(reads, Json.writes[DamageBonus])
}

