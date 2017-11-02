package models.charlist
package weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.bonuses.DmgBns
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class MeleeDamage(
    @ApiModelProperty(required = false, allowableValues = "thr,sw", allowEmptyValue = true)
    attackType: Option[String] = None,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    dmgDice: Int = 0,
    @ApiModelProperty(required = false)
    dmgMod: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "0.1,0.2,0.5,1,2,3,5,10,100")
    armorDiv: Double = 1,
    @ApiModelProperty(
      required = true,
      allowableValues = "cr,cr ex,cut,imp,pi-,pi,pi+,pi++,burn,burn ex,tox,cor,aff,fat,spec.")
    dmgType: String = DamageType.CRUSHING,
    @ApiModelProperty(required = false, value = "Generated string.")
    dmgString: String = "") {
  def calc(thr: => (Int, Int), sw: => (Int, Int), bonuses: Seq[DmgBns]): MeleeDamage = {
    import AttackType._
    import DamageType._
    lazy val (mDice: Int, mMod: Int) = attackType match {
      case Some(THRUSTING) => thr
      case Some(SWINGING)  => sw
      case _               => 0 -> 0
    }
    lazy val dice: Int = dmgDice + mDice
    lazy val mod: Int = dmgMod + mMod + bonuses.map(db => math.max(db.perDie * dice, db.min)).sum
    lazy val diceWMod: Int = dice + (if (mod > 0) mod / 3.5 else 0).toInt
    lazy val modWDice: Int = if (mod > 0) (mod % 3.5).toInt else mod
    copy(dmgString = dmgType match {
      case SPECIAL    => dmgType
      case AFFLICTION => s"HT${modStr(dmgMod)}"
      case _          =>
        s"${attackType map (_ + "+") getOrElse ""}${diceWMod}d${modStr(modWDice)}${divStr(armorDiv)} $dmgType"
    })
  }
}

object MeleeDamage {
  private  val attackTypeErr               =
    JsonValidationError(s"Attack type can only be an empty string or one of: ${AttackType.canBe mkString ", "}")
  private  val armorDivErr                 =
    JsonValidationError(s"Armor divisor can only be one of: ${ArmorDivisor.canBe mkString ", "}")
  private  val dmgTypeErr                  =
    JsonValidationError(s"Damage type can only be one of: ${DamageType.canBe mkString ", "}")
  private  val reads : Reads[MeleeDamage]  = (
    (JsPath \ NameOf.nameOf[MeleeDamage](_.attackType))
      .readNullable[String]
      .filter(attackTypeErr)(_ forall AttackType.canBe) ~
      (JsPath \ NameOf.nameOf[MeleeDamage](_.dmgDice)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[MeleeDamage](_.dmgMod)).readWithDefault(0) ~
      (JsPath \ NameOf.nameOf[MeleeDamage](_.armorDiv)).readWithDefault(1.0).filter(armorDivErr)(ArmorDivisor.canBe) ~
      (JsPath \ NameOf.nameOf[MeleeDamage](_.dmgType)).read[String].filter(dmgTypeErr)(DamageType.canBe) ~
      Reads.pure("")) (MeleeDamage.apply _)
  implicit val format: Format[MeleeDamage] = Format(reads, Json.writes[MeleeDamage])
}
