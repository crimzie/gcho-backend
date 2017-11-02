package models.charlist
package skill

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Technique(
    name: String,
    skill: String,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(required = true, allowableValues = "E,A,H,VH,W")
    diff: String = SkillDifficulty.AVERAGE,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    style: Option[String] = None,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    defLvl: Int = 0,
    var maxLvl: Int = 0,
    var relLvl: Int = 0,
    @ApiModelProperty(required = false)
    notes: String = "",
    @ApiModelProperty(required = false, value = "Calculated value, will be overwritten.")
    lvl: Int = 0) extends Feature {
  if (maxLvl <= defLvl) maxLvl = defLvl + 1
  if (relLvl < defLvl) relLvl = defLvl else if (relLvl > maxLvl) relLvl = maxLvl
  @ApiModelProperty(required = false, value = "Generated value.")
  val tchString = s"$name ($skill${spc map (" (" + _ + ")") getOrElse ""})"
  @ApiModelProperty(required = false, value = "Calculated value.")
  val cp: Int   = relLvl - defLvl + (if (diff == SkillDifficulty.HARD && relLvl > defLvl) 1 else 0)
  @ApiModelProperty(hidden = true)
  override val str: String = tchString

  def updated(skillLvl: Int): Technique = copy(lvl = skillLvl + relLvl)
}

object Technique {
  private  val diffErr                   =
    JsonValidationError(s"Technique difficulty can only be one of: ${SkillDifficulty.techniqueCanBe mkString ", "}")
  private  val reads : Reads[Technique]  = (
    (JsPath \ NameOf.nameOf[Technique](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Technique](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[Technique](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[Technique](_.diff)).read[String].filter(diffErr)(SkillDifficulty.techniqueCanBe) ~
      (JsPath \ NameOf.nameOf[Technique](_.style)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[Technique](_.defLvl)).read(Reads max 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Technique](_.maxLvl)).read[Int] ~
      (JsPath \ NameOf.nameOf[Technique](_.relLvl)).read[Int] ~
      (JsPath \ NameOf.nameOf[Technique](_.notes)).readWithDefault("") ~
      Reads.pure(0)) (Technique.apply _)
  private  val writes: Writes[Technique] = (
    Json.writes[Technique] ~
      (JsPath \ NameOf.nameOf[Technique](_.tchString)).write[String] ~
      (JsPath \ NameOf.nameOf[Technique](_.cp)).write[Int]) (t => (t, t.tchString, t.cp))
  implicit val format: Format[Technique] = Format(reads, writes)
}
