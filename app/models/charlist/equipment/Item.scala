package models.charlist
package equipment

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Item(
    name: String,
    @ApiModelProperty(
      required = false,
      allowableValues = "Ready,Equipped,Combat,Travel,Stash",
      value = "Default: Stash")
    state: String = ItemState.STASH,
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
    cost: Double = 0,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]")
    n: Int = 1) extends Possession with Feature {
  if (hpLeft < -hp * 5) hpLeft = -hp * 5 else if (hpLeft > hp) hpLeft = hp
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalWt  : Double = wt * n
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalCost: Double = cost * n
  @ApiModelProperty(hidden = true)
  override val str: String = name
}

object Item {
  private  val stateErr               =
    JsonValidationError(s"Item state can only be one of: ${ItemState.canBe mkString ", "}")
  private  val hpReads: Reads[Int]    = (JsPath \ NameOf.nameOf[Item](_.hp)).read(Reads min 1 orElse Reads.pure(1))
  private  val reads  : Reads[Item]   = (
    (JsPath \ NameOf.nameOf[Item](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Item](_.state)).readWithDefault(ItemState.STASH).filter(stateErr)(ItemState.canBe) ~
      (JsPath \ NameOf.nameOf[Item](_.dr)).read(Reads min 0 orElse Reads.pure(0)) ~
      hpReads ~
      (JsPath \ NameOf.nameOf[Item](_.hpLeft)).read[Int].orElse(hpReads) ~
      (JsPath \ NameOf.nameOf[Item](_.broken)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Item](_.lc))
        .read(Reads.min(0).orElse(Reads.pure(0)) <~ Reads.max(5) orElse Reads.pure(5)) ~
      (JsPath \ NameOf.nameOf[Item](_.tl))
        .read(Reads.max(12).orElse(Reads.pure(12)) <~ Reads.min(0) orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Item](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Item](_.wt)).read(Reads min 0.0 orElse Reads.pure(0.0)) ~
      (JsPath \ NameOf.nameOf[Item](_.cost)).read(Reads min 0.0 orElse Reads.pure(0.0)) ~
      (JsPath \ NameOf.nameOf[Item](_.n)).read(Reads min 0 orElse Reads.pure(0))) (Item.apply _)
  private  val writes : OWrites[Item] = (
    Json.writes[Item] ~
      (JsPath \ NameOf.nameOf[Item](_.totalWt)).write[Double] ~
      (JsPath \ NameOf.nameOf[Item](_.totalCost)).write[Double]) (it => (it, it.totalWt, it.totalCost))
  implicit val format : Format[Item]  = Format(reads, writes)
}
