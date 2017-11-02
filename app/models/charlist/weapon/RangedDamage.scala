package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RangedDamage(
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    dmgDice: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "range[1, infinity]")
    diceMult: Int = 1,
    @ApiModelProperty(required = false)
    dmgMod: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "0.1,0.2,0.5,1,2,3,5,10,100")
    armorDiv: Double = 1,
    @ApiModelProperty(
      required = true,
      allowableValues = "cr,cr ex,cut,imp,pi-,pi,pi+,pi++,burn,burn ex,tox,cor,aff,fat,spec.")
    dmgType: String = DamageType.CRUSHING,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    fragDice: Int = 0) {
  @ApiModelProperty(required = false, value = "Generated string.")
  val dmgString: String = dmgType match {
    case DamageType.SPECIAL    => dmgType
    case DamageType.AFFLICTION => s"HT${modStr(dmgMod)}"
    case _                     =>
      val fragStr = if (fragDice > 0) " [" + fragDice + "d]" else ""
      s"${dmgDice}d${multStr(diceMult)}${modStr(dmgMod)}${divStr(armorDiv)} $dmgType$fragStr"
  }
}

object RangedDamage {
  private  val armorDivErr                   =
    JsonValidationError(s"Armor divisor can only be one of: ${ArmorDivisor.canBe mkString ", "}.")
  private  val dmgTypeErr                    =
    JsonValidationError(s"Damage type can be only one of: ${DamageType.canBe mkString ", "}.")
  private  val reads : Reads[RangedDamage]   =
    ((JsPath \ NameOf.nameOf[RangedDamage](_.dmgDice)).read[Int](Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedDamage](_.diceMult)).readWithDefault(1)(Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[RangedDamage](_.dmgMod)).readWithDefault(0) ~
      (JsPath \ NameOf.nameOf[RangedDamage](_.armorDiv)).readWithDefault(1.0).filter(armorDivErr)(ArmorDivisor.canBe) ~
      (JsPath \ NameOf.nameOf[RangedDamage](_.dmgType)).read[String].filter(dmgTypeErr)(DamageType.canBe) ~
      (JsPath \ NameOf.nameOf[RangedDamage](_.fragDice))
        .readWithDefault(0)(Reads min 0 orElse Reads.pure(0))) (RangedDamage.apply _)
  private  val writes: OWrites[RangedDamage] = (Json.writes[RangedDamage] ~
    (JsPath \ NameOf.nameOf[RangedDamage](_.dmgString)).write[String]) (rd => (rd, rd.dmgString))
  implicit val format: Format[RangedDamage]  = Format(reads, writes)
}
