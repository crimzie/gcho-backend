package models.charlist
package armor

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.equipment.{ItemState, Possession}
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Armor(
    name: String,
    @ApiModelProperty(required = false, allowableValues = "Ready,Equipped,Combat,Travel,Stash")
    state: String = ItemState.EQUIPPED,
    @ApiModelProperty(required = false, allowableValues = "range[0, 3]")
    db: Int = 0,
    components: Seq[ArmorComponent] = Seq(ArmorComponent()),
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
  if (hpLeft > hp) hpLeft = hp

  override def totalCost: Double = cost

  override def totalWt: Double = wt

  @ApiModelProperty(hidden = true) override val str: String = name
}

object Armor {
  private  val stateErr               =
    JsonValidationError(s"Item state can only be one of: ${ItemState.canBe mkString ", "}")
  private  val hpReads: Reads[Int]    = (JsPath \ NameOf.nameOf[Armor](_.hp)).read(Reads min 1 orElse Reads.pure(1))
  private  val reads  : Reads[Armor]  = (
    (JsPath \ NameOf.nameOf[Armor](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Armor](_.state)).readWithDefault(ItemState.EQUIPPED).filter(stateErr)(ItemState.canBe) ~
      (JsPath \ NameOf.nameOf[Armor](_.db))
        .read(Reads.max(3).orElse(Reads pure 3) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Armor](_.components)).read[Seq[ArmorComponent]] ~
      hpReads ~
      (JsPath \ NameOf.nameOf[Armor](_.hpLeft)).read[Int].orElse(hpReads) ~
      (JsPath \ NameOf.nameOf[Armor](_.broken)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Armor](_.lc))
        .read(Reads.min(0).orElse(Reads.pure(0)) <~ Reads.max(5) orElse Reads.pure(5)) ~
      (JsPath \ NameOf.nameOf[Armor](_.tl))
        .read(Reads.max(12).orElse(Reads.pure(12)) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Armor](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Armor](_.wt)).read(Reads min 0.0 orElse Reads.pure(0.0)) ~
      (JsPath \ NameOf.nameOf[Armor](_.cost)).read(Reads min 0.0 orElse Reads.pure(0.0))) (Armor.apply _)
  implicit val format : Format[Armor] = Format(reads, Json.writes[Armor])
}
