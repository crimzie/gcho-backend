package models.charlist
package weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.equipment.{ItemState, Possession}
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.postfixOps

case class Weapon(
    name: String,
    @ApiModelProperty(required = true, allowableValues = "Ready,Equipped,Combat,Travel,Stash")
    state: String = ItemState.STASH,
    @ApiModelProperty(required = false)
    attacksMelee: Seq[MeleeAttack] = Nil,
    @ApiModelProperty(required = false)
    attacksRanged: Seq[RangedAttack] = Nil,
    @ApiModelProperty(required = false)
    blocks: Seq[BlockDefence] = Nil,
    @ApiModelProperty(required = false)
    offHand: Boolean = false,
    @ApiModelProperty(required = true, allowableValues = "range[-infinity, 0]")
    bulk: Int = 0,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    dr: Int = 0,
    @ApiModelProperty(required = true, allowableValues = "range[1, infinity]")
    hp: Int = 1,
    @ApiModelProperty(required = false)
    var hpLeft: Int = 1,
    @ApiModelProperty(required = false)
    broken: Boolean = false,
    @ApiModelProperty(required = true, allowableValues = "range[0, 5]")
    lc: Int = 5,
    @ApiModelProperty(required = true, allowableValues = "range[0, 12]")
    tl: Int = 0,
    @ApiModelProperty(required = false)
    notes: String = "",
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    wt: Double = 0,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    cost: Double = 0) extends Possession with Feature {
  if (hpLeft < -hp * 5) hpLeft = -hp * 5 else if (hpLeft > hp) hpLeft = hp

  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalWt  : Double      = wt + attacksRanged.map(_.shots.totalWt).sum
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalCost: Double      = cost + attacksRanged.map(_.shots.totalCost).sum
  @ApiModelProperty(required = false, value = "Generated value.")
  val grips    : Seq[String] = attacksMelee.map(_.grip) ++ attacksRanged.map(_.grip) ++ blocks.map(_.grip) distinct
  @ApiModelProperty(hidden = true)
  override val str: String = name
}

object Weapon {
  private  val stateErr                     =
    JsonValidationError(s"Item state can only be one of: ${ItemState.canBe mkString ", "}")
  private  val stateFilter: Reads[String]   = Reads.filter(stateErr)(ItemState.canBe)
  private  val hpReads    : Reads[Int]      =
    (JsPath \ NameOf.nameOf[Weapon](_.hp)).read(Reads min 1 orElse Reads.pure(1))
  private  val reads      : Reads[Weapon]   = (
    (JsPath \ NameOf.nameOf[Weapon](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Weapon](_.state)).read[String](stateFilter) ~
      (JsPath \ NameOf.nameOf[Weapon](_.attacksMelee)).readWithDefault[Seq[MeleeAttack]](Nil) ~
      (JsPath \ NameOf.nameOf[Weapon](_.attacksRanged)).readWithDefault[Seq[RangedAttack]](Nil) ~
      (JsPath \ NameOf.nameOf[Weapon](_.blocks)).readWithDefault[Seq[BlockDefence]](Nil) ~
      (JsPath \ NameOf.nameOf[Weapon](_.offHand)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Weapon](_.bulk)).read(Reads max 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Weapon](_.dr)).read(Reads min 0 orElse Reads.pure(0)) ~
      hpReads ~
      (JsPath \ NameOf.nameOf[Weapon](_.hpLeft)).read[Int].orElse(hpReads) ~
      (JsPath \ NameOf.nameOf[Weapon](_.broken)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Weapon](_.lc))
        .read(Reads.min(0).orElse(Reads.pure(0)) <~ Reads.max(5) orElse Reads.pure(5)) ~
      (JsPath \ NameOf.nameOf[Weapon](_.tl))
        .read(Reads.max(12).orElse(Reads.pure(12)) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Weapon](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Weapon](_.wt)).read(Reads min 0.0 orElse Reads.pure(0.0)) ~
      (JsPath \ NameOf.nameOf[Weapon](_.cost)).read(Reads min 0.0 orElse Reads.pure(0.0))) (Weapon.apply _)
  private  val writes     : OWrites[Weapon] = (
    Json.writes[Weapon] ~
      (JsPath \ NameOf.nameOf[Weapon](_.totalWt)).write[Double] ~
      (JsPath \ NameOf.nameOf[Weapon](_.totalCost)).write[Double] ~
      (JsPath \ NameOf.nameOf[Weapon](_.grips)).write[Seq[String]]) (w => (w, w.totalWt, w.totalCost, w.grips))
  implicit val format     : Format[Weapon]  = Format(reads, writes)
}
