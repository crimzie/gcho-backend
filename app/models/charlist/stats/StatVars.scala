package models.charlist
package stats

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json, Reads}

case class StatVars(
    @ApiModelProperty(required = false, value = "Calculated value.") frightCheck: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") vision: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") hearing: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") tasteSmell: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") touch: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") thrDmg: String = "",
    @ApiModelProperty(required = false, value = "Calculated value.") swDmg: String = "",
    @ApiModelProperty(required = false, value = "Calculated value.") bl: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") combatEncumbrance: String = "",
    @ApiModelProperty(required = false, value = "Calculated value.") travelEncumbrance: String = "",
    @ApiModelProperty(required = false, value = "Calculated value.") combMove: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") travMove: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") dodge: Int = 0,
    @ApiModelProperty(required = false, value = "Calculated value.") sm: Int = 0) {
  private val encLvl: Double => Int = {
    case d if d < 1.01  => 0
    case d if d < 2.01  => 1
    case d if d < 3.01  => 2
    case d if d < 6.01  => 3
    case d if d < 10.01 => 4
    case _              => 5
  }

  def cEnc(combWt: Double): Int = if (bl > 0) encLvl(combWt / bl) else if (combWt > 0) 5 else 0

  def tEnc(travWt: Double): Int = if (bl > 0) encLvl(travWt / bl) else if (travWt > 0) 5 else 0

  def updated(combWt: Double, travWt: Double, bm: Int, bd: Int): StatVars = {
    val encStr = "None" +: "Light" +: "Medium" +: "Heavy" +: "Extra-Heavy" +: "Overencumbered" +: Nil
    val c = cEnc(combWt)
    val t = tEnc(travWt)
    copy(
      combMove = (bm * .2 * (5 - c)).toInt,
      travMove = (bm * .2 * (5 - t)).toInt,
      dodge = bd - c,
      combatEncumbrance = encStr(c),
      travelEncumbrance = encStr(t))
  }
}

object StatVars {
  implicit val format: Format[StatVars] = Format(Reads.pure(Charlist.defStatVars), Json.writes[StatVars])
}
