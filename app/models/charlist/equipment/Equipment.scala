package models.charlist
package equipment

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.armor.Armor
import models.charlist.weapon.Weapon
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.postfixOps

case class Equipment(
    @ApiModelProperty(required = false)
    weapons: Seq[Weapon] = Nil,
    @ApiModelProperty(required = false)
    armor: Seq[Armor] = Nil,
    @ApiModelProperty(required = false)
    items: Seq[Item] = Nil,
    @ApiModelProperty(required = false, dataType = "Boolean")
    combat: Boolean = false) {

  import ItemState._

  private val weights: (String => Boolean) => Double =
    f => weapons ++ armor ++ items collect { case p if f(p.state) => p.totalWt } sum
  private val equip  : Possession => Boolean         =
    p => Set(READY, EQUIPPED)(p.state) && !p.broken
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalCost  : Double = (0.0 /: (weapons ++ armor ++ items)) (_ + _.totalCost)
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalCombWt: Double = weights(Set(READY, EQUIPPED, COMBAT))
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalTravWt: Double = totalCombWt + weights(_ == TRAVEL)
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalDb    : Int    =
    weapons.withFilter(equip).flatMap(_.blocks).map(_.db) ++ armor.withFilter(equip).map(_.db) sum
}

object Equipment {
  private  val reads : Reads[Equipment]  = (
    (JsPath \ NameOf.nameOf[Equipment](_.weapons)).readWithDefault[Seq[Weapon]](Nil) ~
      (JsPath \ NameOf.nameOf[Equipment](_.armor)).readWithDefault[Seq[Armor]](Nil) ~
      (JsPath \ NameOf.nameOf[Equipment](_.items)).readWithDefault[Seq[Item]](Nil) ~
      (JsPath \ NameOf.nameOf[Equipment](_.combat)).readWithDefault(false)) (Equipment.apply _)
  private  val writes: Writes[Equipment] = (
    Json.writes[Equipment] ~
      (JsPath \ NameOf.nameOf[Equipment](_.totalCost)).write[Double] ~
      (JsPath \ NameOf.nameOf[Equipment](_.totalCombWt)).write[Double] ~
      (JsPath \ NameOf.nameOf[Equipment](_.totalTravWt)).write[Double] ~
      (JsPath \ NameOf.nameOf[Equipment](_.totalDb)).write[Int]) {
    e => (e, e.totalCost, e.totalCombWt, e.totalTravWt, e.totalDb)
  }
  implicit val format: Format[Equipment] = Format(reads, writes)
}
