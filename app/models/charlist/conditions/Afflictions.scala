package models.charlist
package conditions

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class Afflictions(
    @ApiModelProperty(required = false) coughing: Boolean = false,
    @ApiModelProperty(required = false) drowsy: Boolean = false,
    @ApiModelProperty(required = false) drunk: Boolean = false,
    @ApiModelProperty(required = false) euphoria: Boolean = false,
    @ApiModelProperty(required = false) nauseated: Boolean = false,
    @ApiModelProperty(required = false) pain: Boolean = false,
    @ApiModelProperty(required = false) tipsy: Boolean = false,
    @ApiModelProperty(required = false) agony: Boolean = false,
    @ApiModelProperty(required = false) choking: Boolean = false,
    @ApiModelProperty(required = false) daze: Boolean = false,
    @ApiModelProperty(required = false) ecstasy: Boolean = false,
    @ApiModelProperty(required = false) hallucinating: Boolean = false,
    @ApiModelProperty(required = false) paralysis: Boolean = false,
    @ApiModelProperty(required = false) retching: Boolean = false,
    @ApiModelProperty(required = false) seizure: Boolean = false,
    @ApiModelProperty(required = false) coma: Boolean = false,
    @ApiModelProperty(required = false) heartAttack: Boolean = false)

object Afflictions {
  private  val reads : Reads[Afflictions]  = (
    (JsPath \ NameOf.nameOf[Afflictions](_.coughing)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.drowsy)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.drunk)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.euphoria)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.nauseated)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.pain)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.tipsy)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.agony)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.choking)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.daze)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.ecstasy)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.hallucinating)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.paralysis)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.retching)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.seizure)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.coma)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[Afflictions](_.heartAttack)).readWithDefault(false)) (Afflictions.apply _)
  implicit val format: Format[Afflictions] = Format(reads, Json.writes[Afflictions])
}
