package models.charlist.weapon

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class RangedRoF(
    @ApiModelProperty(required = false, allowableValues = "range[1, infinity]")
    rof: Int = 1,
    @ApiModelProperty(required = false, allowableValues = "range[1, infinity]")
    rofMult: Int = 1,
    @ApiModelProperty(required = false)
    rofFA: Boolean = false,
    @ApiModelProperty(required = false)
    jet: Boolean = false) {
  @ApiModelProperty(required = false, value = "Generated string.")
  val rofString = s"$rof${multStr(rofMult)}${if (rof != 1 && rofFA) "!" else ""}" // TODO: "Jet" if jet is true
}

object RangedRoF {
  private  val reads : Reads[RangedRoF]   =
    ((JsPath \ NameOf.nameOf[RangedRoF](_.rof)).readWithDefault(1)(Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[RangedRoF](_.rofMult)).readWithDefault(1)(Reads min 1 orElse Reads.pure(1)) ~
      (JsPath \ NameOf.nameOf[RangedRoF](_.rofFA)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[RangedRoF](_.jet)).readWithDefault(false)) (RangedRoF.apply _)
  private  val writes: OWrites[RangedRoF] = (Json.writes[RangedRoF] ~
    (JsPath \ NameOf.nameOf[RangedRoF](_.rofString)).write[String]) (rr => (rr, rr.rofString))
  implicit val format: Format[RangedRoF]  = Format(reads, writes)
}
