package models.charlist

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}

case class CharacterPoints(
    cp: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") stats: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") adv: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") dis: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") skills: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") unspent: Int = 0) {
  def calc(stats: Int, adv: Int, dis: Int, skills: Int): CharacterPoints =
    copy(stats = stats, adv = adv, dis = dis, skills = skills, unspent = cp - skills - stats - adv - dis)
}

object CharacterPoints {
  private  val reads : Reads[CharacterPoints]  = ((JsPath \ NameOf.nameOf[CharacterPoints](_.cp)).read[Int] ~
    Reads.pure(0) ~ Reads.pure(0) ~ Reads.pure(0) ~ Reads.pure(0) ~ Reads.pure(0)) (CharacterPoints.apply _)
  implicit val format: Format[CharacterPoints] = Format(reads, Json.writes[CharacterPoints])
}
