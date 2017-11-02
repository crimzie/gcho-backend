package models.charlist
package dr

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class DrSet(
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]") dr: Int = 0,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]") ep: Int = 0,
    @ApiModelProperty(required = true, allowableValues = "range[0, infinity]") epi: Int = 0) {
  def +(that: DrSet): DrSet = DrSet(this.dr + that.dr, this.ep + that.ep, this.epi + that.epi)

  def *(x: Int): DrSet = DrSet(this.dr * x, this.ep * x, this.epi * x)
}

object DrSet {
  private  val reads : Reads[DrSet]  = (
    (JsPath \ NameOf.nameOf[DrSet](_.dr)).read(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[DrSet](_.ep)).read(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[DrSet](_.epi)).read(Reads min 0 orElse Reads.pure(0))) (DrSet.apply _)
  implicit val format: Format[DrSet] = Format(reads, Json.writes[DrSet])
}
