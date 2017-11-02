package models.charlist
package skill

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.bonuses.{DamageBonus, HasBonusDamage, ReactionBonus}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Skill(
    name: String,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(required = false, dataType = "Int", allowEmptyValue = true)
    tl: Option[Int] = None,
    @ApiModelProperty(required = true, allowableValues = "ST,IQ,DX,HT,Will,Per")
    attr: String = SkillBaseAttribute.DX,
    @ApiModelProperty(required = true, allowableValues = "E,A,H,VH,W")
    diff: String = SkillDifficulty.EASY,
    @ApiModelProperty(required = false)
    defaults: Seq[String] = Nil,
    @ApiModelProperty(required = false)
    prerequisites: Seq[String] = Nil,
    @ApiModelProperty(required = false)
    dmgBonuses: Seq[DamageBonus] = Nil,
    @ApiModelProperty(required = false)
    reactBonuses: Seq[ReactionBonus] = Nil,
    @ApiModelProperty(required = false)
    encumbr: Boolean = false,
    @ApiModelProperty(required = false)
    categories: Seq[String] = Nil,
    @ApiModelProperty(required = false)
    notes: String = "",
    var relLvl: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.")
    bonus: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.")
    lvl: Int = 0) extends HasBonusDamage with Feature {
  @ApiModelProperty(required = false, value = "Generated value.")
  val skillString: String = name + tl.map(s"/TL" + _).getOrElse("") + spc.map(s" (" + _ + ")").getOrElse("")
  if (relLvl < SkillDifficulty.values(diff)) relLvl = SkillDifficulty values diff
  private val l = relLvl - (SkillDifficulty values diff) + 1
  @ApiModelProperty(required = false, value = "Calculated value.")
  val cp: Int = l * 4 - (if (l > 2) 8 else if (l > 1) 6 else 3)

  override val on : Boolean = true
  @ApiModelProperty(hidden = true)
  override val str: String  = skillString

  def calc(bonus: Int, attrVal: Int, enc: => Int): Skill =
    copy(bonus = bonus, lvl = attrVal + relLvl - (if (encumbr) enc else 0) + bonus)
}

object Skill {
  private  val attrErr               =
    JsonValidationError(s"Skill base attribute can only be one of: ${SkillBaseAttribute.canBe mkString ", "}")
  private  val diffErr               =
    JsonValidationError(s"Skill difficulty can only be one of: ${SkillDifficulty.canBe mkString ", "}")
  private  val reads : Reads[Skill]  = (
    (JsPath \ NameOf.nameOf[Skill](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Skill](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[Skill](_.tl))
        .readNullable(Reads.max(12).orElse(Reads.pure(12)) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Skill](_.attr)).read[String].filter(attrErr)(SkillBaseAttribute.canBe) ~
      (JsPath \ NameOf.nameOf[Skill](_.diff)).read[String].filter(diffErr)(SkillDifficulty.canBe) ~
      (JsPath \ NameOf.nameOf[Skill](_.defaults)).readWithDefault[Seq[String]](Nil) ~
      (JsPath \ NameOf.nameOf[Skill](_.prerequisites)).readWithDefault[Seq[String]](Nil) ~
      (JsPath \ NameOf.nameOf[Skill](_.dmgBonuses)).readWithDefault[Seq[DamageBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[Skill](_.reactBonuses)).readWithDefault[Seq[ReactionBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[Skill](_.encumbr)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Skill](_.categories)).readWithDefault[Seq[String]](Nil) ~
      (JsPath \ NameOf.nameOf[Skill](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Skill](_.relLvl)).read[Int] ~
      Reads.pure(0) ~
      Reads.pure(0)) (Skill.apply _)
  private  val writes: Writes[Skill] = (
    Json.writes[Skill] ~
      (JsPath \ NameOf.nameOf[Skill](_.skillString)).write[String] ~
      (JsPath \ NameOf.nameOf[Skill](_.cp)).write[Int]) (s => (s, s.skillString, s.cp))
  implicit val format: Format[Skill] = Format(reads, writes)
}
