package models.charlist
package reaction

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class ReactionMod(
    @ApiModelProperty(required = true, allowableValues = "7,10,13,16")
    freq: Int = 16,
    mod: Int = 0,
    @ApiModelProperty(required = false)
    notes: String = "")

object ReactionMod {
  private  val freqErr                     =
    JsonValidationError(s"Reaction recognition frequency can only be one of: ${ReactionFrequency.canBe mkString ", "}")
  private  val reads : Reads[ReactionMod]  = (
    (JsPath \ NameOf.nameOf[ReactionMod](_.freq)).read[Int].filter(freqErr)(ReactionFrequency.canBe) ~
      (JsPath \ NameOf.nameOf[ReactionMod](_.mod)).read[Int] ~
      (JsPath \ NameOf.nameOf[ReactionMod](_.notes)).readWithDefault("")) (ReactionMod.apply _)
  implicit val format: Format[ReactionMod] = Format(reads, Json.writes[ReactionMod])
}
