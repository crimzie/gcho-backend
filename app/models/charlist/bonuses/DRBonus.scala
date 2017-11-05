package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.armor.HitLocation
import models.charlist.dr.DrSet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class DRBonus(
    @ApiModelProperty(
      required = true,
      allowableValues = "eyes,skull,face,head,neck,right leg,left leg,legs,right arm,left arm,arms,chest,vitals,abdomen,groin,torso,hands,left hand,right hand,feet,right foot,left foot,skin,full body")
    locations: Seq[String] = Seq(HitLocation.SKIN),
    @ApiModelProperty(required = false, value = "Default: false")
    perLvl: Boolean = false,
    @ApiModelProperty(required = false, value = "Default: true")
    front: Boolean = true,
    @ApiModelProperty(required = false, value = "Default: true")
    back: Boolean = true,
    protection: DrSet = DrSet())

object DRBonus {
  private  val locationsErr            =
    JsonValidationError(s"Hit locations can only be some of: ${HitLocation.canBe mkString ", "}")
  private  val reads : Reads[DRBonus]  = (
    (JsPath \ NameOf.nameOf[DRBonus](_.locations)).read[Seq[String]].filter(locationsErr)(_ forall HitLocation.canBe) ~
      (JsPath \ NameOf.nameOf[DRBonus](_.perLvl)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[DRBonus](_.front)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[DRBonus](_.back)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[DRBonus](_.protection)).read[DrSet]) (DRBonus.apply _)
  implicit val format: Format[DRBonus] = Format(reads, Json.writes[DRBonus])
}
