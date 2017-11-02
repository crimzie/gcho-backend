package models.charlist
package stats

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class StatsCurrent(
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]") hpLost: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]") fpLost: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") reeling: Boolean = false,
    @ApiModelProperty(required = false, value = "Calculated value.") tired: Boolean = false,
    @ApiModelProperty(required = false, value = "Calculated value.") collapsing: Boolean = false,
    @ApiModelProperty(required = false, value = "Calculated value.") fallingAslp: Boolean = false,
    @ApiModelProperty(required = false, value = "Calculated value.") vision: Int = 0,  // TODO: calc
    @ApiModelProperty(required = false, value = "Calculated value.") hearing: Int = 0, // TODO: calc
    @ApiModelProperty(required = false, value = "Calculated value.") move: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") dodge: Int = 0) {
  def updated(hp: Int, fp: Int, mov: Int, ddg: Int): StatsCurrent = {
    val clps = hp <= hpLost
    val fslp = fp <= fpLost
    val m = if (clps || fslp) .5 else 1.0
    copy(
      reeling = hp * (2.0 / 3.0) < hpLost,
      tired = fp * (2.0 / 3.0) < fpLost,
      collapsing = clps,
      fallingAslp = fslp,
      move = Charlist rndUp (mov * m),
      dodge = Charlist rndUp (ddg * m))
  }
}

object StatsCurrent {
  private  val reads : Reads[StatsCurrent]  = (
    (JsPath \ NameOf.nameOf[StatsCurrent](_.hpLost)).readWithDefault(0)(Reads min 0) ~
      (JsPath \ NameOf.nameOf[StatsCurrent](_.fpLost)).readWithDefault(0)(Reads min 0) ~
      Reads.pure(false) ~
      Reads.pure(false) ~
      Reads.pure(false) ~
      Reads.pure(false) ~
      (JsPath \ NameOf.nameOf[StatsCurrent](_.vision)).readWithDefault(0) ~
      (JsPath \ NameOf.nameOf[StatsCurrent](_.hearing)).readWithDefault(0) ~
      Reads.pure(0) ~
      Reads.pure(0)) (StatsCurrent.apply _)
  implicit val format: Format[StatsCurrent] = Format(reads, Json.writes[StatsCurrent])
}
