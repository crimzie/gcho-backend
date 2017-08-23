package models
package charlist

import scala.language.postfixOps
import scala.math.max
import scala.util.Random

case class Trait(
                  name: String = "",
                  var traitString: String = "",
                  var types: Seq[String] = Seq(TraitType.PHYSICAL),
                  var category: String = TraitCategory.ADVANTAGE,
                  var switch: String = TraitSwitch.ALWAYSON,
                  ref: String = "",
                  notes: String = "",
                  prerequisites: Seq[String] = Nil, // For future functionality
                  var active: Boolean = true,
                  modifiers: Seq[TraitModifier] = Nil,
                  cpBase: Int = 0,
                  var level: Int = 0,
                  cpPerLvl: Int = 0,
                  var cp: Int = 0) {
  if (types forall TraitType.canBe) () else types = Seq(TraitType.PHYSICAL)
  if (TraitCategory canBe category) () else category = TraitCategory.ADVANTAGE
  if (TraitSwitch canBe switch) () else switch = TraitSwitch.ALWAYSON
  if (switch == TraitSwitch.ALWAYSON) active = true
  for {(_, seq) <- modifiers filter (_.cat == TraitModifierCategory.VARIANT) groupBy (_.variants)} {
    if (seq.count(_.on) != 1) { // TODO: imperative rewrites here; probably worth reimplementing in funct style
      seq.head.on = true
      for (m <- seq.tail) m.on = false
    }
  }
  if (level < 0) level = 0
  private val actNames = modifiers withFilter { m => m.on && m.name != "" } map (_.name) mkString "; "
  traitString =
    name + (if (actNames == "") "" else s" ($actNames)") + (if (level > 0 || cpPerLvl != 0) s" $level" else "")

  import TraitModifierAffects._

  private case class MCost(pts: Int = 0, pct: Int = 0, mlt: Double = 1) {
    def +(that: (Int, Int, Double)): MCost = MCost(pts + that._1, pct + that._2, mlt * that._3)
  }

  private val cpMods = modifiers filter (_.on) groupBy (_.affects) mapValues {
    seq => (MCost() /: seq) (_ + _.costVal)
  } withDefaultValue MCost()
  private val MCost(bPts, bPct, bMlt) = cpMods(BASE)
  private val MCost(lPts, lPct, lMlt) = cpMods(LEVELS)
  private val MCost(tPts, tPct, tMlt) = cpMods(TOTAL)
  cp = Charlist rndUp ((bPts + cpBase) * max(.2, bPct * .01 + 1) * bMlt
    + level * (lPts + cpPerLvl) * max(.2, lPct * .01 + 1)
    * lMlt + tPts) * max(.2, tPct * .01 + 1) * tMlt

  val attrBonusValues: Seq[BonusAttribute] = if (active) {
    for (b <- modifiers withFilter (_.on) flatMap (_.attrBonuses))
      yield if (b.perLvl) b copy (bonus = b.bonus * level) else b
  } else Nil
  val skillBonusValue: (String, String) => Int = (s: String, spc: String) =>
    if (active) (modifiers withFilter (_.on) flatMap (_.skillBonuses) collect {
      case b: BonusSkill if NameCompare.fit(s, spc, b) => if (b.perLvl) b.bonus * level else b.bonus
    }).sum
    else 0
  val drBonuses: Seq[BonusDR] = if (active) {
    for (b <- modifiers withFilter (_.on) flatMap (_.drBonuses))
      yield if (b.perLvl) b copy (protection = b.protection * level) else b
  } else Nil
  val attrCostMods: Seq[BonusAttributeCost] = modifiers withFilter (_.on) flatMap (_.attrCostMods)
  val reactBonuses: Seq[BonusReaction] = if (active) {
    for (b <- modifiers withFilter (_.on) flatMap (_.reactBonuses))
      yield if (b.perLvl) b copy (bonus = b.bonus * level) else b
  } else Nil
}

case class FlaggedTrait(
                         _id: String = Random.alphanumeric take 8 mkString,
                         ready: Boolean,
                         data: Trait,
                         override var category: String = "",
                         override var name: String = "",
                         override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Trait, FlaggedTrait] {
  category = FlaggedFeature.TRAITS
  name = data.name

  override def updated(user: String): FlaggedTrait = copy(user = user)
}

case class TraitModifier(
                          override var on: Boolean = true,
                          var cat: String = TraitModifierCategory.MODIFIER,
                          variants: String = "",
                          name: String = "",
                          ref: String = "",
                          notes: String = "",
                          attrBonuses: Seq[BonusAttribute] = Nil,
                          skillBonuses: Seq[BonusSkill] = Nil,
                          dmgBonuses: Seq[BonusDamage] = Nil,
                          drBonuses: Seq[BonusDR] = Nil,
                          attrCostMods: Seq[BonusAttributeCost] = Nil,
                          reactBonuses: Seq[BonusReaction] = Nil,
                          var affects: String = TraitModifierAffects.TOTAL,
                          var costType: String = TraitModifierCostType.PERCENT,
                          var level: Int = 0,
                          cost: Double = 0) extends DamageBonusing {
  if (cat == TraitModifierCategory.DEFAULT) on = true
  if (TraitModifierCategory canBe cat) () else cat = TraitModifierCategory.MODIFIER
  if (TraitModifierAffects canBe affects) () else affects = TraitModifierAffects.TOTAL
  if (TraitModifierCostType canBe costType) () else costType = TraitModifierCostType.PERCENT
  if (level < 0) level = 0

  import TraitModifierCostType._

  val costVal: (Int, Int, Double) = costType match {
    case POINTS => (cost.toInt, 0, 1)
    case PERCENT => (0, cost.toInt, 1)
    case LEVEL => (0, cost.toInt * level, 1)
    case MULTIPLIER => (0, 0, cost)
  }
}

object TraitSwitch {
  val ALWAYSON = "Always on"
  val SWITCHABLE = "Switchable"
  val CONTROL = "Roll"
  val ATTACK = "Attack"
  val canBe = Set(ALWAYSON, SWITCHABLE, CONTROL, ATTACK)
}

object TraitType {
  val MENTAL = "Mental"
  val PHYSICAL = "Physical"
  val SOCIAL = "Social"
  val MUNDANE = "Mundane"
  val EXOTIC = "Exotic"
  val SUPER = "Supernatural"
  val canBe = Set(MENTAL, PHYSICAL, SOCIAL, MUNDANE, EXOTIC, SUPER)
}

object TraitCategory {
  val RACE = "Race"
  val ADVANTAGE = "Advantage"
  val DISADVANTAGE = "Disadvantage"
  val PERK = "Perk"
  val QUIRK = "Quirk"
  val LANGUAGE = "Language"
  val canBe = Set(RACE, ADVANTAGE, DISADVANTAGE, PERK, QUIRK, LANGUAGE)
}

object TraitModifierCategory {
  val DEFAULT = "base"
  val MODIFIER = "modifier"
  val VARIANT = "variant"
  val canBe = Set(DEFAULT, MODIFIER, VARIANT)
}

object TraitModifierAffects {
  val TOTAL = "total"
  val BASE = "base"
  val LEVELS = "levels only"
  val canBe = Set(TOTAL, BASE, LEVELS)
}

object TraitModifierCostType {
  val PERCENT = "percentage"
  val LEVEL = "percentage per level"
  val POINTS = "points"
  val MULTIPLIER = "multiplier"
  val canBe = Set(PERCENT, LEVEL, POINTS, MULTIPLIER)
}
