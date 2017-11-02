package models.charlist
package dr

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, Reads}

case class DamageResistance(
    @ApiModelProperty(required = false) skull: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) eyes: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) face: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) neck: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) armLeft: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) armRight: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) handLeft: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) handRight: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) chest: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) vitals: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) abdomen: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) groin: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) legLeft: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) legRight: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) footLeft: HitLocationDR = HitLocationDR(),
    @ApiModelProperty(required = false) footRight: HitLocationDR = HitLocationDR())

object DamageResistance {
  implicit val format: Format[DamageResistance] =
    Format(Reads.pure(Charlist.defDamageResistance), Json.writes[DamageResistance])
}
