package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class MeleeAttack(
    name: String,
    @ApiModelProperty(required = false)
    available: Boolean = true,
    @ApiModelProperty(required = false)
    grip: String = "Default",
    damage: MeleeDamage = MeleeAttack.md,
    @ApiModelProperty(required = false)
    followup: Seq[MeleeDamage] = Nil,
    @ApiModelProperty(required = false)
    linked: Seq[MeleeDamage] = Nil,
    skill: String,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    spc: Option[String] = None,
    @ApiModelProperty(required = false)
    parry: Int = 0,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    parryType: Option[String] = None,
    @ApiModelProperty(
      required = false,
      allowableValues = "range[1, infinity]",
      allowEmptyValue = true)
    st: Option[Int] = None,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    hands: Option[String] = None,
    reach: String,
    @ApiModelProperty(required = false)
    notes: String = "") {
  @ApiModelProperty(required = false, value = "Generated string.")
  val parryString: String = if (parryType contains "No") parryType.get else s"$parry${parryType getOrElse ""}"
  @ApiModelProperty(required = false, value = "Generated string.")
  val stString            = s"${st getOrElse ""}${hands getOrElse ""}"
}

object MeleeAttack {
  private  val md                           = MeleeDamage()
  private  val reads : Reads[MeleeAttack]   = (
    (JsPath \ NameOf.nameOf[MeleeAttack](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.available)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.grip)).readWithDefault("Default") ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.damage)).read[MeleeDamage] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.followup)).readWithDefault[Seq[MeleeDamage]](Nil) ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.linked)).readWithDefault[Seq[MeleeDamage]](Nil) ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.skill)).read[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.spc)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.parry)).readWithDefault(0) ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.parryType)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.st)).readNullable[Int](Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.hands)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.reach)).read[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.notes)).readWithDefault("")) (MeleeAttack.apply _)
  private  val writes: OWrites[MeleeAttack] = (
    Json.writes[MeleeAttack] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.stString)).write[String] ~
      (JsPath \ NameOf.nameOf[MeleeAttack](_.parryString)).write[String]) (ma => (ma, ma.stString, ma.parryString))
  implicit val format: Format[MeleeAttack]  = Format(reads, writes)
}
