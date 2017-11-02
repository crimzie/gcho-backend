package models.charlist
package conditions

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class Conditions(
    @ApiModelProperty(required = false)
    unconscious: Boolean = false,
    @ApiModelProperty(required = false)
    mortallyWounded: Boolean = false,
    @ApiModelProperty(required = false)
    dead: Boolean = false,
    @ApiModelProperty(required = false, allowableValues = "0,1,2,3,4,6,8")
    shock: Int = 0,
    @ApiModelProperty(required = false)
    stunned: Boolean = false,
    @ApiModelProperty(required = false)
    afflictions: Afflictions = Afflictions(),
    @ApiModelProperty(
      required = false,
      allowableValues = "Standing,Crouching,Sitting,Kneeling,Crawling,Prone,On Back",
      value = "Default: Standing")
    posture: String = Posture.STANDING,
    @ApiModelProperty(required = false)
    closeCombat: Boolean = false,
    @ApiModelProperty(required = false)
    grappled: Boolean = false,
    @ApiModelProperty(required = false)
    pinned: Boolean = false,
    @ApiModelProperty(required = false)
    sprinting: Boolean = false,
    @ApiModelProperty(required = false)
    mounted: Boolean = false)

object Conditions {
  private val postureErr = JsonValidationError(s"Posture can only be one of: ${Posture.canBe mkString ", "}")
  private val shockErr   = JsonValidationError(s"Shock can only be one of: ${Shock.canBe mkString ", "}")

  private  val reads : Reads[Conditions]  = (
    (JsPath \ NameOf.nameOf[Conditions](_.unconscious)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.mortallyWounded)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.dead)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.shock)).readWithDefault(0).filter(shockErr)(Shock.canBe) ~
      (JsPath \ NameOf.nameOf[Conditions](_.stunned)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.afflictions)).readWithDefault(Afflictions()) ~
      (JsPath \ NameOf.nameOf[Conditions](_.posture))
        .readWithDefault(Posture.STANDING)
        .filter(postureErr)(Posture.canBe) ~
      (JsPath \ NameOf.nameOf[Conditions](_.closeCombat)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.grappled)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.pinned)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.sprinting)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Conditions](_.mounted)).readWithDefault(false)) (Conditions.apply _)
  implicit val format: Format[Conditions] = Format(reads, Json.writes[Conditions])
}
