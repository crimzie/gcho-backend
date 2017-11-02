package models.charlist
package `trait`

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.bonuses._
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.math.max

case class Trait(
    name: String,
    @ApiModelProperty(
      required = false,
      value = "May be a subset of: Mental, Physical, Social, Mundane, Exotic, Supernatural")
    types: Seq[String] = Nil,
    @ApiModelProperty(required = true, allowableValues = "Race,Advantage,Disadvantage,Perk,Quirk,Language")
    category: String = TraitCategory.ADVANTAGE,
    @ApiModelProperty(
      required = false,
      allowableValues = "Always on,Switchable,Roll,Attack",
      value = "Default: Always on")
    switch: String = TraitSwitch.ALWAYSON,
    @ApiModelProperty(required = false)
    ref: String = "",
    @ApiModelProperty(required = false)
    notes: String = "",
    @ApiModelProperty(required = false)
    prerequisites: Seq[String] = Nil,
    @ApiModelProperty(required = false, value = "Default: true")
    var active: Boolean = true,
    @ApiModelProperty(required = false)
    var modifiers: Seq[TraitModifier] = Nil,
    @ApiModelProperty(required = false)
    cpBase: Int = 0,
    @ApiModelProperty(
      required = false,
      dataType = "Int",
      allowableValues = "range[0, infinity]",
      allowEmptyValue = true)
    level: Option[Int] = None,
    @ApiModelProperty(required = false, dataType = "Int", allowEmptyValue = true)
    cpPerLvl: Option[Int] = None) extends Feature {
  if (switch == TraitSwitch.ALWAYSON && !active) active = true
  modifiers = modifiers.filter(_.cat == TraitModifierCategory.DEFAULT).take(1) ++
    modifiers.filter(_.cat == TraitModifierCategory.VARIANT).groupBy(_.variants).flatMap { case (_, seq) =>
      if (seq.count(_.on) != 1) seq.head.copy(on = true) +: (for {m <- seq.tail} yield m.copy(on = false)) else seq
    }.toSeq ++
    modifiers.filter(_.cat == TraitModifierCategory.MODIFIER)

  private val actNames = modifiers withFilter { m => m.on && m.name != "" } map (_.name) mkString "; "
  @ApiModelProperty(required = false, value = "Generated string.")
  val traitString: String = name + (if (actNames.isEmpty) "" else s" ($actNames)") +
    (if (level.nonEmpty || cpPerLvl.exists(_ != 0)) s" ${level getOrElse 0}" else "")
  @ApiModelProperty(hidden = true)
  override val str: String = traitString

  import TraitModifierAffects._

  private case class MCost(pts: Int = 0, pct: Int = 0, mlt: Double = 1) {
    def +(that: (Int, Int, Double)): MCost = MCost(pts + that._1, pct + that._2, mlt * that._3)
  }

  private val cpMods: Map[String, MCost] =
    modifiers filter (_.on) groupBy (_.affects) mapValues (s => (MCost() /: s) (_ + _.costVal)) withDefaultValue MCost()
  private val MCost(bPts, bPct, bMlt)    = cpMods(BASE)
  private val MCost(lPts, lPct, lMlt)    = cpMods(LEVELS)
  private val MCost(tPts, tPct, tMlt)    = cpMods(TOTAL)
  private val lev                        = level getOrElse 0
  @ApiModelProperty(required = false, value = "Calculated value.")      // TODO: category check 
  val cp: Int = Charlist rndUp ((bPts + cpBase) * max(.2, bPct * .01 + 1) * bMlt
    + lev * (lPts + cpPerLvl.getOrElse(0)) * max(.2, lPct * .01 + 1) * lMlt + tPts) * max(.2, tPct * .01 + 1) * tMlt

  @ApiModelProperty(hidden = true) val skillBonusValue: (String, Option[String]) => Int =
    (s, spc) => if (active) modifiers.withFilter(_.on).flatMap(_.skillBonuses).collect {
      case b: SkillBonus if NameCompare.fit(s, spc, b) => if (b.perLvl) b.bonus * lev else b.bonus
    }.sum else 0

  @ApiModelProperty(hidden = true) val attrBonusValues: Seq[AttributeBonus] = if (active) {
    for (b <- modifiers withFilter (_.on) flatMap (_.attrBonuses))
      yield if (b.perLvl) b copy (bonus = b.bonus * lev) else b
  } else Nil
  @ApiModelProperty(hidden = true) val drBonuses      : Seq[DRBonus]        = if (active) {
    for {b <- modifiers withFilter (_.on) flatMap (_.drBonuses)}
      yield if (b.perLvl) b copy (protection = b.protection * lev) else b
  } else Nil
  @ApiModelProperty(hidden = true) val reactBonuses   : Seq[ReactionBonus]  = if (active) {
    for (b <- modifiers withFilter (_.on) flatMap (_.reactBonuses))
      yield if (b.perLvl) b copy (bonus = b.bonus * lev) else b
  } else Nil

  @ApiModelProperty(hidden = true) val attrCostMods: Seq[AttributeCostBonus] =
    modifiers withFilter (_.on) flatMap (_.attrCostMods)
}

object Trait {
  private  val typesErr              =
    JsonValidationError(s"Trait types can only be some of: ${TraitType.canBe mkString ", "}")
  private  val categoryErr           =
    JsonValidationError(s"Trait category can only be one of: ${TraitCategory.canBe mkString ", "}")
  private  val switchErr             =
    JsonValidationError(s"Trait switch can only be one of: ${TraitSwitch.canBe mkString ", "}")
  private  val reads : Reads[Trait]  = (
    (JsPath \ NameOf.nameOf[Trait](_.name)).read[String] ~
      (JsPath \ NameOf.nameOf[Trait](_.types))
        .readWithDefault[Seq[String]](Nil)
        .filter(typesErr)(_ forall TraitType.canBe) ~
      (JsPath \ NameOf.nameOf[Trait](_.category)).read[String].filter(categoryErr)(TraitCategory.canBe) ~
      (JsPath \ NameOf.nameOf[Trait](_.switch))
        .readWithDefault(TraitSwitch.ALWAYSON)
        .filter(switchErr)(TraitSwitch.canBe) ~
      (JsPath \ NameOf.nameOf[Trait](_.ref)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Trait](_.notes)).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Trait](_.prerequisites)).readWithDefault[Seq[String]](Nil) ~
      (JsPath \ NameOf.nameOf[Trait](_.active)).readWithDefault(true) ~
      (JsPath \ NameOf.nameOf[Trait](_.modifiers)).readWithDefault[Seq[TraitModifier]](Nil) ~
      (JsPath \ NameOf.nameOf[Trait](_.cpBase)).readWithDefault(0) ~
      (JsPath \ NameOf.nameOf[Trait](_.level)).readNullable(Reads min 0 orElse Reads.pure(0)) ~
      (JsPath \ NameOf.nameOf[Trait](_.cpPerLvl)).readNullable[Int]) (Trait.apply _)
  implicit val format: Format[Trait] = Format(reads, Json.writes[Trait])
}
