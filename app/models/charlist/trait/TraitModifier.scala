package models.charlist
package `trait`

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.bonuses._
import play.api.libs.functional.syntax._
import play.api.libs.json._

case class TraitModifier(
    @ApiModelProperty(required = false, value = "Default: true")
    override val on: Boolean = true,
    @ApiModelProperty(required = true, allowableValues = "base,modifier,variant")
    cat: String = TraitModifierCategory.MODIFIER,
    @ApiModelProperty(required = false, allowEmptyValue = true)
    variants: Option[String] = None,
    name: String,
    @ApiModelProperty(required = false)
    ref: String = "",
    @ApiModelProperty(required = false)
    notes: String = "",
    @ApiModelProperty(required = false)
    attrBonuses: Seq[AttributeBonus] = Nil,
    @ApiModelProperty(required = false)
    skillBonuses: Seq[SkillBonus] = Nil,
    @ApiModelProperty(required = false)
    dmgBonuses: Seq[DamageBonus] = Nil,
    @ApiModelProperty(required = false)
    drBonuses: Seq[DRBonus] = Nil,
    @ApiModelProperty(required = false)
    attrCostMods: Seq[AttributeCostBonus] = Nil,
    @ApiModelProperty(required = false)
    reactBonuses: Seq[ReactionBonus] = Nil,
    @ApiModelProperty(
      required = false,
      allowableValues = "total,base,levels only",
      value = "Default: total")
    affects: String = TraitModifierAffects.TOTAL,
    @ApiModelProperty(
      required = false,
      allowableValues = "percentage,percentage per level,points,multiplier",
      value = "Default: points")
    costType: String = TraitModifierCostType.POINTS,
    @ApiModelProperty(
      required = false,
      allowableValues = "range[1, infinity}",
      value = "Default: 1")
    level: Int = 1,
    @ApiModelProperty(required = false, value = "Default: 0")
    cost: Double = 0) extends HasBonusDamage {

  import TraitModifierCostType._

  @ApiModelProperty(hidden = true)
  val costVal: (Int, Int, Double) = costType match {
    case POINTS     => (cost.toInt, 0, 1)
    case PERCENT    => (0, cost.toInt, 1)
    case LEVEL      => (0, cost.toInt * level, 1)
    case MULTIPLIER => (0, 0, cost)
  }
}

object TraitModifier {
  private  val catErr                        =
    JsonValidationError(s"Trait modifier category can only be one of: ${TraitModifierCategory.canBe mkString ", "}")
  private  val affectsErr                    =
    JsonValidationError(s"Trait modifier subject can only be one of: ${TraitModifierAffects.canBe mkString ", "}")
  private  val costTypeErr                   =
    JsonValidationError(s"Trait modifier cost type can only be one of: ${TraitModifierCostType.canBe mkString ", "}")
  private  val reads : Reads[TraitModifier]  = (
    (JsPath \ NameOf.nameOf[TraitModifier](_.on)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.cat)).read[String].filter(catErr)(TraitModifierCategory.canBe) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.variants)).readNullable[String] ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.ref)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.attrBonuses)).readWithDefault[Seq[AttributeBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.skillBonuses)).readWithDefault[Seq[SkillBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.dmgBonuses)).readWithDefault[Seq[DamageBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.drBonuses)).readWithDefault[Seq[DRBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.attrCostMods)).readWithDefault[Seq[AttributeCostBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.reactBonuses)).readWithDefault[Seq[ReactionBonus]](Nil) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.affects))
        .readWithDefault(TraitModifierAffects.TOTAL)
        .filter(affectsErr)(TraitModifierAffects.canBe) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.costType))
        .readWithDefault(TraitModifierCostType.POINTS)
        .filter(costTypeErr)(TraitModifierCostType.canBe) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.level)).readWithDefault(1)(Reads min 1) ~
      (JsPath \ NameOf.nameOf[TraitModifier](_.cost)).readWithDefault(0.0)) (TraitModifier.apply _)
  implicit val format: Format[TraitModifier] = Format(reads, Json.writes[TraitModifier])
}
