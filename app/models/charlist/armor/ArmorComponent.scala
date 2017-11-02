package models.charlist
package armor

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.dr.DrSet
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class ArmorComponent(
    protection: DrSet = DrSet(),
    @ApiModelProperty(required = false)
    front: Boolean = true,
    @ApiModelProperty(required = false)
    back: Boolean = true,
    @ApiModelProperty(required = true, allowableValues = "hard,soft,force field,tough skin")
    drType: String = DrType.HARD,
    @ApiModelProperty(required = true, value = "Can be a subset of: eyes,skull,face,head,neck," +
      "right leg,left leg,legs,right arm,left arm,arms,chest,vitals,abdomen,groin,torso,hands," +
      "left hand,right hand,feet,right foot,left foot,skin,full body")
    locations: Set[String] = Set(HitLocation.BODY))

object ArmorComponent {
  private  val drTypeErr                      =
    JsonValidationError(s"DR type can only be one of: ${DrType.canBe mkString ", "}")
  private  val locationsErr                   =
    JsonValidationError(s"Locations can only be some of: ${HitLocation.canBe mkString ", "}")
  private  val reads : Reads[ArmorComponent]  = (
    (JsPath \ NameOf.nameOf[ArmorComponent](_.protection)).read[DrSet] ~
      (JsPath \ NameOf.nameOf[ArmorComponent](_.front)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[ArmorComponent](_.back)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[ArmorComponent](_.drType)).read[String](Reads.filter(drTypeErr)(DrType.canBe)) ~
      (JsPath \ NameOf.nameOf[ArmorComponent](_.locations))
        .read[Set[String]]
        .filter(locationsErr)(_ forall HitLocation.canBe)) (ArmorComponent.apply _)
  implicit val format: Format[ArmorComponent] = Format(reads, Json.writes[ArmorComponent])
}
