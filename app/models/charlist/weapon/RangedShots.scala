package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RangedShots(
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    shots: Int = 1,
    @ApiModelProperty(required = false)
    reload: String = "",
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    shotsLoaded: Int = 1,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    shotsCarried: Int = 0,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    shotWt: Double = 0,
    @ApiModelProperty(required = false, allowableValues = "range[0, infinity]")
    shotCost: Double = 0) {
  private val str: String = if (shots != 0) s"$shotsLoaded/$shots" else ""
  @ApiModelProperty(required = false, value = "Generated string.")
  val shotsString       = s"$str$reload${if (shotsCarried != 0) " " + shotsCarried else ""}"
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalWt  : Double = (shotsCarried + shotsLoaded) * shotWt
  @ApiModelProperty(required = false, value = "Calculated value.")
  val totalCost: Double = (shotsCarried + shotsLoaded) * shotCost
}

object RangedShots {
  private  val reads : Reads[RangedShots]   =
    ((JsPath \ NameOf.nameOf[RangedShots](_.shots)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedShots](_.reload)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[RangedShots](_.shotsLoaded)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedShots](_.shotsCarried)).readWithDefault(0)(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[RangedShots](_.shotWt)).readWithDefault(0.0)(Reads min 0.0 orElse Reads.pure(0.0)) ~
      (JsPath \ NameOf.nameOf[RangedShots](_.shotCost))
        .readWithDefault(0.0)(Reads min 0.0 orElse Reads.pure(0.0))) (RangedShots.apply _)
  private  val writes: OWrites[RangedShots] = (Json.writes[RangedShots] ~
    (JsPath \ NameOf.nameOf[RangedShots](_.shotsString)).write[String] ~
    (JsPath \ NameOf.nameOf[RangedShots](_.totalCost)).write[Double] ~
    (JsPath \ NameOf.nameOf[RangedShots](_.totalWt)).write[Double]) {
    rs => (rs, rs.shotsString, rs.totalCost, rs.totalWt)
  }
  implicit val format: Format[RangedShots]  = Format(reads, writes)
}
