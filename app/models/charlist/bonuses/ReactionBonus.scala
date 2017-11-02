package models.charlist
package bonuses

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.reaction.ReactionFrequency
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class ReactionBonus(
    affected: String = "Everyone",
    @ApiModelProperty(required = false)
    reputation: Boolean = false,
    @ApiModelProperty(required = false)
    perLvl: Boolean = false,
    @ApiModelProperty(required = true, allowableValues = "16,13,10,7")
    freq: Int = 16,
    bonus: Int = 0,
    @ApiModelProperty(required = false)
    notes: String = "") {
  private val noteConcat = (n: String) => if (n.isEmpty || notes.isEmpty) s"$notes$n" else s"$notes; $n"

  def +(that: ReactionBonus): ReactionBonus = copy(bonus = this.bonus + that.bonus, notes = noteConcat(that.notes))

  def +~(that: ReactionBonus): ReactionBonus =
    copy(bonus = math.max(math.min(this.bonus + that.bonus, 4), -4), notes = noteConcat(that.notes))
}

object ReactionBonus {
  private  val freqErr                       =
    JsonValidationError(s"Recognition frequency can only be one of: ${ReactionFrequency.canBe mkString ", "}")
  private  val reads : Reads[ReactionBonus]  = (
    (JsPath \ NameOf.nameOf[ReactionBonus](_.affected)).read[String] ~
      (JsPath \ NameOf.nameOf[ReactionBonus](_.reputation)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[ReactionBonus](_.perLvl)).readWithDefault(false) ~
      (JsPath \ NameOf.nameOf[ReactionBonus](_.freq)).read[Int].filter(freqErr)(ReactionFrequency.canBe) ~
      (JsPath \ NameOf.nameOf[ReactionBonus](_.bonus)).read[Int] ~
      (JsPath \ NameOf.nameOf[ReactionBonus](_.notes)).readWithDefault("")) (ReactionBonus.apply _)
  implicit val format: Format[ReactionBonus] = Format(reads, Json.writes[ReactionBonus])
}
