package models.charlist

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.armor.HitLocation
import models.charlist.weapon.DamageType
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Wound(
    @ApiModelProperty(
      required = true,
      allowableValues = "eyes,skull,face,head,neck,right leg,left leg,legs,right arm,left arm,arms,chest,vitals,abdomen,groin,torso,hands,left hand,right hand,feet,right foot,left foot,skin,full body")
    location: String = HitLocation.CHEST,
    @ApiModelProperty(
      required = true,
      allowableValues = "cr,cr ex,cut,imp,pi-,pi,pi+,pi++,burn,burn ex,tox,cor,aff,fat,spec.")
    dType: String = DamageType.CRUSHING,
    @ApiModelProperty(required = true, allowableValues = "range[1, infinity]")
    points: Int = 1,
    @ApiModelProperty(required = false)
    firstAid: Boolean = false,
    @ApiModelProperty(required = false)
    bleeding: Boolean = false,
    @ApiModelProperty(required = false)
    crippling: Boolean = false,
    @ApiModelProperty(required = false)
    lasting: Boolean = false)

object Wound {
  private  val locErr                =
    JsonValidationError(s"Wound location can only be one of: ${HitLocation.canBe mkString ", "}")
  private  val dTypeErr              =
    JsonValidationError(s"Damage type can only be one of: ${DamageType.canBe mkString ", "}")
  private  val reads : Reads[Wound]  =
    ((JsPath \ NameOf.nameOf[Wound](_.location)).read[String].filter(locErr)(HitLocation.canBe) ~
      (JsPath \ NameOf.nameOf[Wound](_.dType)).read[String].filter(dTypeErr)(DamageType.canBe) ~
      (JsPath \ NameOf.nameOf[Wound](_.points)).read(Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[Wound](_.firstAid)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Wound](_.bleeding)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Wound](_.crippling)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Wound](_.lasting)).readWithDefault(false)) (Wound.apply _)
  implicit val format: Format[Wound] = Format(reads, Json.writes[Wound])
}
