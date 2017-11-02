package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.Reads._
import play.api.libs.json._

case class RangedAttack(
    name: String,
    @ApiModelProperty(required = false)
    available: Boolean = true,
    @ApiModelProperty(required = false)
    grip: String = "Default",
    damage: RangedDamage = RangedAttack.rd,
    @ApiModelProperty(required = false)
    followup: Seq[RangedDamage] = Nil,
    @ApiModelProperty(required = false)
    linked: Seq[RangedDamage] = Nil,
    skill: String,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    acc: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    accMod: Int = 0,
    @ApiModelProperty(required = false)
    rng: String = "",
    @ApiModelProperty(required = false)
    rof: RangedRoF = RangedAttack.rr,
    @ApiModelProperty(required = true, allowableValues = "range[1, infinity]")
    rcl: Int = 2,
    shots: RangedShots = RangedAttack.rs,
    @ApiModelProperty(required = false, allowableValues = "range[1, infinity]", allowEmptyValue = true)
    st: Option[Int] = None,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    hands: Option[String] = None,
    @ApiModelProperty(required = false, allowableValues = "range[4, 18]")
    malf: Int = 18,
    @ApiModelProperty(required = false)
    notes: String = "") {
  @ApiModelProperty(required = false, value = "Generated string.")
  val stString = s"${st getOrElse ""}${hands getOrElse ""}"
}

object RangedAttack {
  private  val rd                            = RangedDamage()
  private  val rr                            = RangedRoF()
  private  val rs                            = RangedShots()
  private  val reads : Reads[RangedAttack]   = (
    (JsPath \ NameOf.nameOf[RangedAttack](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.available)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.grip)).readWithDefault("Default") ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.damage)).read[RangedDamage] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.followup)).readWithDefault[Seq[RangedDamage]](Nil) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.linked)).readWithDefault[Seq[RangedDamage]](Nil) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.acc)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.accMod)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.rng)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.rof)).readWithDefault(rr) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.rcl)).read[Int](Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.shots)).read[RangedShots] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.st)).readNullable[Int](Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.hands)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.malf))
        .readWithDefault(18)(Reads.min(4).orElse(Reads.pure(4)) <~ Reads.max(18) orElse Reads.pure(18)) ~
      (JsPath \ NameOf.nameOf[RangedAttack](_.notes)).readWithDefault("")) (RangedAttack.apply _)
  private  val writes: OWrites[RangedAttack] = (Json.writes[RangedAttack] ~
    (JsPath \ NameOf.nameOf[RangedAttack](_.stString)).write[String]) (ra => (ra, ra.stString))
  implicit val format: Format[RangedAttack]  = Format(reads, writes)
}
