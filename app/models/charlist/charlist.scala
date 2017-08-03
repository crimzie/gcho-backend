package models.charlist

import play.api.libs.json.{Json, OFormat}

import scala.collection.breakOut
// TODO: OOP
/**
  * Case class for conversion between JSON input and database entries and for calculation and validation of
  * charlist's inner secondary values on construction. Initialization automatically recalculates input values, producing
  * its instance with correct data.
  */
case class Charlist(// TODO: maybe make recalc functions in compliance with functional programming style
                    _id: String = "",
                    timestamp: String = "",
                    player: String = "",
                    access: Seq[String] = Nil, // For future functionality
                    name: String = "New Character",
                    cp: CharacterPoints = CharacterPoints(),
                    description: Description = Description(),
                    stats: Stats = Stats(),
                    statVars: StatVars = StatVars(),
                    var damageResistance: DamageResistance = DamageResistance(),
                    var reactions: Seq[Reaction] = Nil,
                    traits: Seq[Trait] = Seq(
                      Trait(name = "Human", category = TraitCategory.RACE, modifiers = Seq(TraitModifier(
                        drBonuses = Seq(BonusDR(Seq(HitLocation.SKULL), protection = DrSet(2))))))),
                    skills: Seq[Skill] = Nil,
                    techniques: Seq[Technique] = Nil,
                    equip: Equipment = Equipment(),
                    currentStats: StatsCurrent = StatsCurrent(),
                    wounds: Seq[Wound] = Nil,
                    conditions: Conditions = Conditions(),
                    var api: String = "") {
  api = "0.3.4"

  {
    import BonusToAttribute._
    import HitLocation._

    val drMap = {
      (for {
        b <- traits flatMap (_.drBonuses)
        l <- b.locations flatMap locMap
      } yield (l, if (b.front) b.protection else DrSet(), if (b.back) b.protection else DrSet())) ++
        (for {
          c <- equip.armor flatMap (_.components)
          l <- c.locations flatMap locMap
        } yield (l, if (c.front) c.protection else DrSet(), if (c.back) c.protection else DrSet()))
    } groupBy {
      case (location, _, _) => location
    } mapValues {
      tupl3s => HitLocationDR((DrSet() /: tupl3s) (_ + _._2), (DrSet() /: tupl3s) (_ + _._3))
    } withDefaultValue HitLocationDR(DrSet(), DrSet())
    damageResistance = DamageResistance(drMap(SKULL), drMap(EYES), drMap(FACE), drMap(NECK), drMap(ARM_LEFT),
      drMap(ARM_RIGHT), drMap(HAND_LEFT), drMap(HAND_RIGHT), drMap(CHEST), drMap(VITALS), drMap(ABDOMEN), drMap(GROIN),
      drMap(LEG_LEFT), drMap(LEG_RIGHT), drMap(FOOT_LEFT), drMap(FOOT_RIGHT))

    val bonuses = traits flatMap (_.attrBonusValues) groupBy (_.attr) mapValues {
      seq => (0.0 /: seq) (_ + _.bonus)
    } withDefaultValue 0.0
    stats.st.calaulateVal(10, bonuses(ST).toInt)
    stats.dx.calaulateVal(10, bonuses(DX).toInt)
    stats.iq.calaulateVal(10, bonuses(IQ).toInt)
    stats.ht.calaulateVal(10, bonuses(HT).toInt)
    stats.will.calaulateVal(10, bonuses(WILL).toInt)
    stats.per.calaulateVal(10, bonuses(PER).toInt)
    stats.basicSpeed.calaulateVal((stats.dx.value + stats.ht.value) * .25, bonuses(BASIC_SPEED))
    stats.basicMove.calaulateVal(stats.basicSpeed.value.toInt, bonuses(BASIC_MOVE).toInt)
    stats.hp.calaulateVal(stats.st.value, bonuses(HP).toInt)
    stats.fp.calaulateVal(stats.ht.value, bonuses(FP).toInt)
    stats.liftSt.calaulateVal(stats.st.value, bonuses(LIFT).toInt)
    stats.strikeSt.calaulateVal(stats.st.value, bonuses(STRIKE).toInt)
    statVars.sm = bonuses(SM).toInt
    statVars.frightCheck = math.min(13, stats.will.value + bonuses(FC).toInt)
    statVars.vision = stats.per.value + bonuses(VISION).toInt
    statVars.hearing = stats.per.value + bonuses(HEARING).toInt
    statVars.tasteSmell = stats.per.value + bonuses(TASTE_SMELL).toInt
    statVars.touch = stats.per.value + bonuses(TOUCH).toInt
    statVars.bl = (stats.liftSt.value * stats.liftSt.value * .2).toInt
    statVars.calculate(equip.totalCombWt, equip.totalTravWt, stats.basicMove.value,
      stats.basicSpeed.value.toInt + 3 + bonuses(DODGE).toInt)
    currentStats.calculate(stats.hp.value, stats.fp.value, statVars.combMove, statVars.dodge)
  }
  {
    import SkillBaseAttribute._

    val attrVal = Map(
      ST -> stats.st.value,
      DX -> stats.dx.value,
      IQ -> stats.iq.value,
      HT -> stats.ht.value,
      WILL -> stats.will.value,
      PER -> stats.per.value)
    for (s <- skills) {
      s.bonus = (0 /: traits) (_ + _.skillBonusValue(s.name, s.spc))
      s.calculateLvl(attrVal(s.attr), statVars.cEnc)
    }
    val sklCache = collection.mutable.Map[(String, String), Int]()
    for (t <- techniques) {
      val l = sklCache getOrElseUpdate(
        (t.skill, t.spc),
        skills.collectFirst {
          case Skill(t.skill, t.spc, _, _, _, _, _, _, _, _, _, _, _, _, _, _, lvl) => lvl
        }.getOrElse(0))
      t.calculateLvl(l)
    }

    reactions =
      (for {(affected, seq) <- traits.flatMap(_.reactBonuses) ++ skills.flatMap(_.reactBonuses) groupBy (_.affected)}
        yield Reaction(
          affected,
          (for {(freq, seq2) <- seq groupBy (_.freq)}
            yield {
              val bonusReaction =
                ((for {(reputation, seq3) <- seq2 groupBy (_.reputation)}
                  yield (seq3 :\ BonusReaction()) (if (reputation) _ +~ _ else _ + _)) :\ BonusReaction()) (_ + _)
              ReactionMod(freq, bonusReaction.bonus, bonusReaction.notes)
            }) (breakOut))) (breakOut) // TODO: make freq-bonus sums

    val (thrD, thrM) = stats.strikeSt.value match {
      case x if x < 1 => (0, 0)
      case x if x < 11 => (1, ((x - 1) / 2) - 6)
      case x => ((x - 3) / 8, ((x - 3) / 2) % 4 - 1)
    }
    val (swD, swM) = stats.strikeSt.value match {
      case x if x < 1 => (0, 0)
      case x if x < 9 => (1, ((x - 1) / 2) - 5)
      case x => ((x - 5) / 4, (x - 5) % 4 - 1)
    }
    val dStr = (d: Int, m: Int) => s"${d}d${if (m < 0) m else if (m > 0) "+" + m else ""}"
    statVars.thrDmg = dStr(thrD, thrM)
    statVars.swDmg = dStr(swD, swM)
    val bnsDmgCache = collection.mutable.Map[(String, String), DmgBns]()
    for (MeleeAttack(_, _, _, dmg, flw, lnk, s, sp, _, _, _, _, _, _, _) <- equip.weapons flatMap (_.attacksMelee)) {
      val bns = bnsDmgCache getOrElseUpdate(
        (s, sp),
        skills collectFirst {
          case Skill(`s`, `sp`, _, _, _, _, _, _, _, _, _, _, _, _, _, relLvl, _) => relLvl
        } match {
          case Some(l) =>
            (DmgBns() /: (skills ++ traits.withFilter(_.active).flatMap(_.modifiers))) (_ + _.dmgBVal(s, sp, l))
          case None => DmgBns()
        })
      dmg.calculateDmg((thrD, thrM), (swD, swM), bns)
      for (m <- flw ++ lnk) m.calculateDmg((thrD, thrM), (swD, swM), DmgBns())
    }

    val costMods = traits flatMap (_.attrCostMods) groupBy (_.attr) mapValues {
      seq => (0 /: seq) (_ + _.cost)
    } withDefaultValue 0
    stats.st.cpMod = 100 + costMods(ST)
    stats.dx.cpMod = 100 + costMods(DX)
    stats.iq.cpMod = 100 + costMods(IQ)
    stats.ht.cpMod = 100 + costMods(HT)
    stats.st.calculateCp(10)
    stats.dx.calculateCp(20)
    stats.iq.calculateCp(20)
    stats.ht.calculateCp(10)
    stats.will.calculateCp(5)
    stats.per.calculateCp(5)
    stats.liftSt.calculateCp(3)
    stats.strikeSt.calculateCp(5)
    stats.hp.calculateCp(2)
    stats.fp.calculateCp(3)
    stats.basicSpeed.calculateCp(20)
    stats.basicMove.calculateCp(5)
    cp.stats = stats.cp
    val (aCp, dCp) = traits map (_.cp) partition (_ > 0)
    cp.adv = aCp.sum
    cp.dis = dCp.sum
    cp.skills = skills.map(_.cp).sum + techniques.map(_.cp).sum
    cp.unspent = cp.cp - cp.skills - cp.stats - cp.adv - cp.dis
  }
}

/**
  * Charlist sub namespace for json header field name strings.
  **/
object CharlistFields {
  val ID = "_id"
  val TIMESTAMP = "timestamp"
  val PLAYER = "player"
  val NAME = "name"
}

case class CharacterPoints(
                            cp: Int = 0,
                            var stats: Int = 0,
                            var adv: Int = 0,
                            var dis: Int = 0,
                            var skills: Int = 0,
                            var unspent: Int = 0)

case class Description(
                        age: String = "",
                        height: String = "",
                        weight: String = "",
                        bio: String = "")

case class Stats(
                  st: StatInt = StatInt(),
                  dx: StatInt = StatInt(),
                  iq: StatInt = StatInt(),
                  ht: StatInt = StatInt(),
                  will: StatInt = StatInt(),
                  per: StatInt = StatInt(),
                  liftSt: StatInt = StatInt(),
                  strikeSt: StatInt = StatInt(),
                  hp: StatInt = StatInt(),
                  fp: StatInt = StatInt(),
                  basicSpeed: StatDouble = StatDouble(),
                  basicMove: StatInt = StatInt()) {
  def cp: Int = (0 /: Seq(st, dx, iq, ht, will, per, liftSt, strikeSt, hp, fp, basicSpeed, basicMove)) (_ + _.cp)
}

/**
  * Container for calculated secondary values.
  **/
case class StatVars(
                     var frightCheck: Int = 0,
                     var vision: Int = 0,
                     var hearing: Int = 0,
                     var tasteSmell: Int = 0,
                     var touch: Int = 0,
                     var thrDmg: String = "",
                     var swDmg: String = "",
                     var bl: Int = 0,
                     var combatEncumbrance: String = "",
                     var travelEncumbrance: String = "",
                     var combMove: Int = 0,
                     var travMove: Int = 0,
                     var dodge: Int = 0,
                     var sm: Int = 0) {
  var cEnc = 0
  var tEnc = 0

  def calculate(combWt: Double, travWt: Double, bm: Int, bd: Int): this.type = {
    val encLvl = (x: Double) => x match {
      case d if d < 1.01 => 0
      case d if d < 2.01 => 1
      case d if d < 3.01 => 2
      case d if d < 6.01 => 3
      case d if d < 10.01 => 4
      case _ => 5
    }
    cEnc = if (bl > 0) encLvl(combWt / bl) else if (combWt > 0) 5 else 0
    tEnc = if (bl > 0) encLvl(travWt / bl) else if (travWt > 0) 5 else 0
    combMove = (bm * .2 * (5 - cEnc)).toInt
    travMove = (bm * .2 * (5 - tEnc)).toInt
    dodge = bd - cEnc
    val encStr = "None" +: "Light" +: "Medium" +: "Heavy" +: "Extra-Heavy" +: "Overencumbered" +: Nil
    combatEncumbrance = encStr(cEnc)
    travelEncumbrance = encStr(tEnc)
    this
  }
}

case class StatsCurrent(
                         var hpLost: Int = 0,
                         var fpLost: Int = 0,
                         var reeling: Boolean = false,
                         var tired: Boolean = false,
                         var collapsing: Boolean = false,
                         var fallingAslp: Boolean = false,
                         var vision: Int = 0,
                         var hearing: Int = 0,
                         var move: Int = 0,
                         var dodge: Int = 0) {
  if (hpLost < 0) hpLost = 0
  if (fpLost < 0) fpLost = 0

  def calculate(hp: Int, fp: Int, mov: Int, ddg: Int): this.type = {
    reeling = hp * (2.0 / 3.0) < hpLost
    tired = fp * (2.0 / 3.0) < fpLost
    collapsing = hp <= hpLost
    fallingAslp = fp <= fpLost
    val m = if (collapsing || fallingAslp) .5 else 1.0
    move = Charlist rndUp (mov * m)
    dodge = Charlist rndUp (ddg * m)
    this
  }
}

sealed abstract class Stat[A <: AnyVal](implicit x: scala.math.Numeric[A]) {

  import x._

  var delta: A
  var base: A
  var bonus: A
  var cpMod: Int
  var cp: Int

  def value: A = delta + base + bonus

  def calaulateVal(b: A, bns: A): this.type = {
    base = b
    bonus = bns
    if (lt(value, fromInt(0))) delta = fromInt(0) - base - bonus
    this
  }

  def calculateCp(cost: Int): this.type = {
    cp = Charlist rndUp delta.toDouble * cost * math.max(.2, cpMod * .01)
    this
  }
}

case class StatInt(var delta: Int = 0, var base: Int = 0, var bonus: Int = 0, var cpMod: Int = 100, var cp: Int = 0)
  extends Stat[Int]

case class StatDouble(
                       var delta: Double = 0,
                       var base: Double = 0,
                       var bonus: Double = 0,
                       var cpMod: Int = 100,
                       var cp: Int = 0)
  extends Stat[Double]

case class Reaction(affected: String = "", modifiers: Seq[ReactionMod] = Nil)

case class ReactionMod(freq: Int = 16, mod: Int = 0, notes: String = "")

case class DmgBns(perDie: Int = 0, plain: Int = 0) {
  def +(that: DmgBns) = DmgBns(this.perDie + that.perDie, this.plain + that.plain)
}

sealed trait FlaggedFeature

/**
  * Parent uniting all case classes carrying a bonus to melee damage
  **/
sealed trait DamageBonusing {
  val dmgBonuses: Seq[BonusDamage]

  def on: Boolean = true

  def on_=(b: Boolean): Unit = ()

  def dmgBVal(s: => String, spc: => String, lvl: => Int): DmgBns =
    if (on) (DmgBns() /: dmgBonuses.collect {
      case b@BonusDamage(_, _, _, _, rSkl, pDie, bns) if NameCompare.fit(s, spc, b) && rSkl <= lvl =>
        if (pDie) DmgBns(perDie = bns) else DmgBns(plain = bns)
    }) (_ + _)
    else DmgBns()
}

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
  for ((_, seq) <- modifiers filter (_.cat == TraitModifierCategory.VARIANT) groupBy (_.variants)) {
    if (seq.count(_.on) != 1) {
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
  cp = Charlist rndUp ((bPts + cpBase) * math.max(.2, bPct * .01 + 1) * bMlt
    + level * (lPts + cpPerLvl) * math.max(.2, lPct * .01 + 1)
    * lMlt + tPts) * math.max(.2, tPct * .01 + 1) * tMlt

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

case class FlaggedTrait(ready: Boolean, data: Trait) extends FlaggedFeature

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
                          cost: Double = 0)
  extends DamageBonusing {
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

case class Skill(
                  name: String = "",
                  spc: String = "",
                  var tl: Int = 0,
                  var skillString: String = "",
                  var attr: String = SkillBaseAttribute.DX,
                  var diff: String = SkillDifficulty.EASY,
                  defaults: Seq[String] = Nil, // For future functionality
                  prerequisites: Seq[String] = Nil, // For future functionality
                  dmgBonuses: Seq[BonusDamage] = Nil,
                  reactBonuses: Seq[BonusReaction] = Nil,
                  encumbr: Boolean = false,
                  var bonus: Int = 0,
                  categories: Seq[String] = Nil,
                  notes: String = "",
                  var cp: Int = 1,
                  var relLvl: Int = 0,
                  var lvl: Int = 0)
  extends DamageBonusing {
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (SkillBaseAttribute canBe attr) () else attr = SkillBaseAttribute.DX
  if (SkillDifficulty canBe diff) () else diff = SkillDifficulty.EASY
  skillString = name + (if (tl != 0) s"/TL$tl" else "") + (if (spc != "") s" ($spc)" else "")
  if (relLvl < SkillDifficulty.values(diff)) relLvl = SkillDifficulty values diff
  private val l = relLvl - (SkillDifficulty values diff) + 1
  cp = l * 4 - (if (l > 2) 8 else if (l > 1) 6 else 3)

  def calculateLvl(attrVal: Int, enc: Int): Unit = lvl = attrVal + relLvl - (if (encumbr) enc else 0) + bonus
}

case class FlaggedSkill(ready: Boolean, data: Skill) extends FlaggedFeature

case class Technique(
                      name: String = "",
                      skill: String = "",
                      spc: String = "",
                      var tchString: String = "",
                      var diff: String = SkillDifficulty.AVERAGE,
                      style: String = "",
                      defLvl: Int = 0,
                      var maxLvl: Int = 0,
                      notes: String = "",
                      var cp: Int = 0,
                      var relLvl: Int = 0,
                      var lvl: Int = 0) {
  if (SkillDifficulty techniqueCanBe diff) () else diff = SkillDifficulty.AVERAGE
  if (maxLvl <= defLvl) maxLvl = defLvl + 1
  if (relLvl < defLvl) relLvl = defLvl else if (relLvl > maxLvl) relLvl = maxLvl
  tchString = s"$name ($skill${if (spc != "") " (" + spc + ")" else ""})"
  cp = relLvl - defLvl + (if (diff == SkillDifficulty.HARD && relLvl > defLvl) 1 else 0)

  def calculateLvl(skill: Int): Unit = lvl = skill + relLvl
}

case class FlaggedTechnique(ready: Boolean, data: Technique) extends FlaggedFeature

case class BonusAttribute(var attr: String = BonusToAttribute.ST, perLvl: Boolean = false, bonus: Double = 0) {
  if (BonusToAttribute canBe attr) () else attr = BonusToAttribute.ST
}

sealed trait SkillBased {
  val skill: String
  var skillCompare: String
  val spc: String
  var spcCompare: String
}

case class BonusSkill(
                       skill: String = "",
                       var skillCompare: String = NameCompare.IS,
                       spc: String = "",
                       var spcCompare: String = NameCompare.ANY,
                       perLvl: Boolean = false,
                       bonus: Int = 0)
  extends SkillBased {
  if (NameCompare canBe skillCompare) () else skillCompare = NameCompare.IS
  if (NameCompare spcCanBe spcCompare) () else spcCompare = NameCompare.ANY
}

case class BonusDamage(
                        skill: String = "",
                        var skillCompare: String = NameCompare.IS,
                        spc: String = "",
                        var spcCompare: String = NameCompare.ANY,
                        relSkill: Int = 0,
                        perDie: Boolean = false,
                        bonus: Int = 0)
  extends SkillBased {
  if (NameCompare canBe skillCompare) () else skillCompare = NameCompare.IS
  if (NameCompare spcCanBe spcCompare) () else spcCompare = NameCompare.ANY
}

case class BonusDR(
                    var locations: Seq[String] = Seq(HitLocation.SKIN),
                    perLvl: Boolean = false,
                    front: Boolean = true,
                    back: Boolean = true,
                    protection: DrSet = DrSet()) {
  if (locations forall HitLocation.canBe) () else locations = Seq(HitLocation.SKIN)
}

case class BonusAttributeCost(var attr: String = SkillBaseAttribute.ST, cost: Int = 0) {
  if (SkillBaseAttribute canBe attr) () else attr = SkillBaseAttribute.ST
}

case class BonusReaction(
                          affected: String = "",
                          reputation: Boolean = false,
                          perLvl: Boolean = false,
                          var freq: Int = 16,
                          bonus: Int = 0,
                          notes: String = "") {
  if (ReactionFrequency canBe freq) () else freq = 16
  private val noteSum = (n: String) => Seq(notes, n) filter (_ != "") mkString "; "

  def +(that: BonusReaction): BonusReaction = this copy(bonus = this.bonus + that.bonus, notes = noteSum(that.notes))

  def +~(that: BonusReaction): BonusReaction =
    this copy(bonus = math.max(math.min(this.bonus + that.bonus, 4), -4), notes = noteSum(that.notes))
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

object SkillDifficulty {
  val EASY = "E"
  val AVERAGE = "A"
  val HARD = "H"
  val VERY_HARD = "VH"
  val WOW = "W"
  // TODO: wildcard difficulty
  val values: Map[String, Int] = Map(EASY -> 0, AVERAGE -> -1, HARD -> -2, VERY_HARD -> -3, WOW -> -4)
  val canBe = Set(EASY, AVERAGE, HARD, VERY_HARD, WOW)
  val techniqueCanBe = Set(AVERAGE, HARD)
}

object SkillBaseAttribute {
  val ST = "ST"
  val IQ = "IQ"
  val DX = "DX"
  val HT = "HT"
  val WILL = "Will"
  val PER = "Per"
  val canBe = Set(ST, IQ, DX, HT, WILL, PER)
}

object BonusToAttribute {
  val ST = "st"
  val DX = "dx"
  val IQ = "iq"
  val HT = "ht"
  val WILL = "will"
  val FC = "fright check"
  val PER = "perception"
  val VISION = "vision"
  val HEARING = "hearing"
  val TASTE_SMELL = "taste smell"
  val TOUCH = "touch"
  val DODGE = "dodge"
  val PARRY = "parry"
  val BLOCK = "block"
  val BASIC_SPEED = "speed"
  val BASIC_MOVE = "move"
  val HP = "hp"
  val FP = "fp"
  val SM = "sm"
  val LIFT = "lifting st"
  val STRIKE = "striking st"
  val canBe = Set(ST, DX, IQ, HT, WILL, FC, PER, VISION, HEARING, TASTE_SMELL, TOUCH, DODGE, PARRY, BLOCK, BASIC_SPEED,
    BASIC_MOVE, HP, FP, SM, LIFT, STRIKE)
}

object NameCompare {
  val ANY = "is anything"
  val IS = "is"
  val BEGINS = "starts with"
  val CONTAINS = "contains"
  val ISNT = "is not"
  val canBe = Set(IS, BEGINS, CONTAINS)
  val spcCanBe = Set(ANY, IS, BEGINS, CONTAINS, ISNT)
  val fit: (String, String, SkillBased) => Boolean = (s, spc, b) =>
    (b.skillCompare match {
      case IS => s equalsIgnoreCase b.skill
      case BEGINS => s regionMatches(true, 0, b.skill, 0, b.skill.length)
      case CONTAINS => s contains b.skill
    }) && (b.spcCompare match {
      case ANY => true
      case IS => spc equalsIgnoreCase b.spc
      case BEGINS => spc regionMatches(true, 0, b.spc, 0, b.spc.length)
      case CONTAINS => spc contains b.spc
      case ISNT => !(spc equalsIgnoreCase b.spc)
    })
}

object ReactionFrequency {
  val ALLWAYS = 16
  val OFTEN = 13
  val SOMETIMES = 10
  val OCCASIONALLY = 7
  val values: Map[Int, Double] = Map(ALLWAYS -> 1.0, OFTEN -> (2.0 / 3.0), SOMETIMES -> .5, OCCASIONALLY -> (1.0 / 3.0))
  val canBe = Set(ALLWAYS, OFTEN, SOMETIMES, OCCASIONALLY)
}

case class Equipment(
                      weapons: Seq[Weapon] = Nil,
                      armor: Seq[Armor] = Nil,
                      items: Seq[Item] = Nil,
                      var totalDb: Int = 0,
                      var totalCost: Double = 0,
                      var totalCombWt: Double = 0,
                      var totalTravWt: Double = 0,
                      combat: Boolean = false) {

  import ItemState._

  private val weights = (f: String => Boolean) =>
    weapons.filter(!_.innate) ++ armor ++ items collect { case p if f(p.state) => p.totalWt }
  totalCost = (0.0 /: (weapons.filter(!_.innate) ++ armor ++ items)) (_ + _.totalCost)
  totalCombWt = weights(Set(READY, EQUIPPED, COMBAT)).sum
  totalTravWt = totalCombWt + weights(_ == TRAVEL).sum
  private val equip = (p: Possession) => Set(READY, EQUIPPED)(p.state) && !p.broken
  totalDb = (weapons.withFilter(equip).flatMap(_.blocks).map(_.db) ++ armor.withFilter(equip).map(_.db)).sum
}

/**
  * Parent for all "equipment item" classes that simplifies total equipment cost and weight aggregation.
  **/
sealed trait Possession {
  var state: String
  val broken: Boolean

  def totalCost: Double

  def totalWt: Double
}

case class Weapon(
                   name: String = "",
                   var state: String = ItemState.STASH,
                   innate: Boolean = false, // TODO: move to traits?
                   attacksMelee: Seq[MeleeAttack] = Nil,
                   attacksRanged: Seq[RangedAttack] = Nil,
                   blocks: Seq[BlockDefence] = Nil,
                   var grips: Seq[String] = Nil,
                   offHand: Boolean = false,
                   var bulk: Int = 0,
                   var dr: Int = 0,
                   var hp: Int = 1,
                   var hpLeft: Int = 1,
                   broken: Boolean = false,
                   var lc: Int = 5,
                   var tl: Int = 0,
                   notes: String = "",
                   var wt: Double = 0,
                   var cost: Double = 0,
                   var totalWt: Double = 0,
                   var totalCost: Double = 0)
  extends Possession {
  if (ItemState canBe state) () else state = ItemState.STASH
  if (bulk > 0) bulk = 0
  if (dr < 0) dr = 0
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0
  grips = (attacksMelee.map(_.grip) ++ attacksRanged.map(_.grip) ++ blocks.map(_.grip)).distinct
  totalWt = wt + attacksRanged.map(_.shots.totalWt).sum
  totalCost = cost + attacksRanged.map(_.shots.totalCost).sum
}

case class FlaggedWeapon(ready: Boolean, data: Weapon) extends FlaggedFeature

case class MeleeAttack(
                        name: String = "",
                        available: Boolean = true,
                        grip: String = "",
                        damage: MeleeDamage = MeleeDamage(),
                        followup: Seq[MeleeDamage] = Nil,
                        linked: Seq[MeleeDamage] = Nil,
                        skill: String = "",
                        spc: String = "",
                        parry: Int = 0,
                        parryType: String = "",
                        var parryString: String = "",
                        var st: Int = 10,
                        hands: String = "",
                        reach: String = "",
                        notes: String = "") {
  parryString = if (parryType == "No") parryType else "" + parry + parryType
  if (st <= 0) st = 1
}

case class MeleeDamage(
                        var attackType: String = AttackType.WEAPON,
                        var dmgDice: Int = 0,
                        dmgMod: Int = 0,
                        var armorDiv: Double = 1,
                        var dmgType: String = DamageType.CRUSHING,
                        var dmgString: String = "") {
  if (AttackType canBe attackType) () else attackType = AttackType.WEAPON
  if (dmgDice < 0) dmgDice = 0
  if (ArmorDivisor canBe armorDiv) () else armorDiv = 1
  if (DamageType canBe dmgType) () else dmgType = DamageType.CRUSHING

  def calculateDmg(thr: => (Int, Int), sw: => (Int, Int), bonus: DmgBns): this.type = {
    import AttackType._
    val (mDice, mMod) = attackType match {
      case THRUSTING => thr
      case SWINGING => sw
      case WEAPON => (0, 0)
    }
    var dice = dmgDice + mDice
    var mod = dmgMod + mMod + bonus.perDie * dice + bonus.plain
    if (mod > 0) {
      dice += (mod / 3.5).toInt
      mod = (mod % 3.5).toInt
    }
    import DamageType._
    dmgString = dmgType match {
      case SPECIAL => dmgType
      case AFFLICTION => s"HT${if (dmgMod > 0) "+" + dmgMod else if (dmgMod < 0) dmgMod else ""}"
      case _ => (if (attackType == THRUSTING) "thr+" else if (attackType == SWINGING) "sw+" else "") +
        s"${dice}d${if (mod > 0) "+" + mod else if (mod < 0) mod else ""}" +
        s"${if (armorDiv < 1) "(" + armorDiv + ")" else if (armorDiv > 1) "(" + armorDiv.toInt + ")" else ""} $dmgType"
    }
    this
  }
}

object AttackType {
  val THRUSTING = "thr"
  val SWINGING = "sw"
  val WEAPON = ""
  val canBe = Set(THRUSTING, SWINGING, WEAPON)
}

case class RangedAttack(
                         name: String = "",
                         available: Boolean = true,
                         grip: String = "",
                         damage: RangedDamage = RangedDamage(),
                         followup: Seq[RangedDamage] = Nil,
                         linked: Seq[RangedDamage] = Nil,
                         skill: String = "",
                         spc: String = "",
                         jet: Boolean = false,
                         var acc: Int = 0,
                         var accMod: Int = 0,
                         rng: String = "",
                         rof: RangedRoF = RangedRoF(),
                         var rcl: Int = 2,
                         shots: RangedShots = RangedShots(),
                         var st: Int = 10,
                         hands: String = "",
                         var malf: Int = 18,
                         notes: String = "") {
  if (acc < 0) acc = 0
  if (accMod < 0) accMod = 0
  if (rcl <= 0) rcl = 1
  if (st <= 0) st = 1
  if (malf > 18) malf = 18 else if (malf < 4) malf = 4
  if (jet) rof.rofString = "Jet"
}

case class RangedDamage(
                         var dmgDice: Int = 0,
                         var diceMult: Int = 1,
                         dmgMod: Int = 0,
                         var armorDiv: Double = 1,
                         var dmgType: String = DamageType.CRUSHING,
                         var fragDice: Int = 0,
                         var dmgString: String = "") {
  if (dmgDice < 0) dmgDice = 0
  if (diceMult <= 0) diceMult = 1
  if (ArmorDivisor canBe armorDiv) () else armorDiv = 1
  if (DamageType canBe dmgType) () else dmgType = DamageType.CRUSHING
  if (fragDice < 0) fragDice = 0
  dmgString = dmgType match {
    case DamageType.SPECIAL => dmgType
    case DamageType.AFFLICTION => s"HT${if (dmgMod > 0) "+" + dmgMod else if (dmgMod < 0) dmgMod else ""}"
    case _ => s"${dmgDice}d${if (diceMult != 1) "x" + diceMult else ""}" +
      s"${if (dmgMod > 0) "+" + dmgMod else if (dmgMod < 0) dmgMod else ""}" +
      s"${if (armorDiv < 1) "(" + armorDiv + ")" else if (armorDiv > 1) "(" + armorDiv.toInt + ")" else ""}" +
      s" $dmgType${if (fragDice > 0) " [" + fragDice + "d]" else ""}"
  }
}

object ArmorDivisor {
  val canBe = Set(0.1, 0.2, 0.5, 1, 2, 3, 5, 10, 100)
}

object DamageType {
  val CRUSHING = "cr"
  val CRUSHING_EXPLOSION = "cr ex"
  val CUTTING = "cut"
  val IMPALING = "imp"
  val PIERCING_SMALL = "pi-"
  val PIERCING = "pi"
  val PIERCING_LARGE = "pi+"
  val PIERCING_HUGE = "pi++"
  val BURNING = "burn"
  val BURNING_EXPLOSION = "burn ex"
  val TOXIC = "tox"
  val CORROSION = "cor"
  val AFFLICTION = "aff"
  val FATIGUE = "fat"
  val SPECIAL = "spec."
  val canBe = Set(CRUSHING, CRUSHING_EXPLOSION, CUTTING, IMPALING, PIERCING_SMALL, PIERCING, PIERCING_LARGE,
    PIERCING_HUGE, BURNING, BURNING_EXPLOSION, TOXIC, CORROSION, AFFLICTION, FATIGUE, SPECIAL)
}

case class RangedRoF(
                      var rof: Int = 1,
                      var rofMult: Int = 1,
                      rofFA: Boolean = false,
                      var rofString: String = "") {
  if (rof <= 0) rof = 1
  if (rofMult <= 0) rofMult = 1
  rofString = s"$rof${if (rofMult != 1) "x" + rofMult else ""}${if (rofFA) "!" else ""}"
}

case class RangedShots(
                        var shots: Int = 1,
                        reload: String = "",
                        var shotsLoaded: Int = 0,
                        var shotsCarried: Int = 0,
                        var shotWt: Double = 0,
                        var shotCost: Double = 0,
                        var shotsString: String = "",
                        var totalWt: Double = 0,
                        var totalCost: Double = 0) {
  if (shots < 0) shots = 0
  if (shotsLoaded < 0) shotsLoaded = 0 else if (shotsLoaded > shots) shotsLoaded = shots
  if (shotsCarried < 0) shotsCarried = 0
  if (shotWt < 0) shotWt = 0
  if (shotCost < 0) shotCost = 0
  shotsString = s"${if (shots != 0) "" + shotsLoaded + "/" + shots else ""}$reload " +
    s"${if (shotsCarried != 0) shotsCarried else ""}"
  totalWt = (shotsCarried + shotsLoaded) * shotWt
  totalCost = (shotsCarried + shotsLoaded) * shotCost
}

case class BlockDefence(
                         name: String = "",
                         available: Boolean = true,
                         grip: String = "",
                         skill: String = "",
                         spc: String = "",
                         var db: Int = 1,
                         notes: String = "") {
  if (db < 1) db = 1
}

case class Armor(
                  name: String = "",
                  var state: String = ItemState.EQUIPPED,
                  var db: Int = 0,
                  components: Seq[ArmorComponent] = Seq(ArmorComponent()),
                  var hp: Int = 1,
                  var hpLeft: Int = 1,
                  broken: Boolean = false,
                  var lc: Int = 5,
                  var tl: Int = 0,
                  notes: String = "",
                  var wt: Double = 0,
                  var cost: Double = 0)
  extends Possession {
  if (ItemState canBe state) () else state = ItemState.EQUIPPED
  if (db < 0) db = 0 else if (db > 3) db = 3
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0

  override def totalCost: Double = cost

  override def totalWt: Double = wt
}

case class FlaggedArmor(ready: Boolean, data: Armor) extends FlaggedFeature

case class ArmorComponent(
                           var protection: DrSet = DrSet(),
                           var front: Boolean = true,
                           back: Boolean = true,
                           var drType: String = DrType.HARD,
                           var locations: Seq[String] = Nil) {
  if (!front && !back) front = true
  if (DrType canBe drType) () else drType = DrType.HARD
  locations = locations.distinct filter HitLocation.canBe
}

object DrType {
  val HARD = "hard"
  val SOFT = "soft"
  val FIELD = "force field"
  val SKIN = "tough skin"
  val canBe = Set(HARD, SOFT, FIELD, SKIN)
}

object HitLocation {
  val EYES = "eyes"
  val SKULL = "skull"
  val FACE = "face"
  val HEAD = "head"
  val NECK = "neck"
  val LEG_RIGHT = "right leg"
  val LEG_LEFT = "left leg"
  val LEGS = "legs"
  val ARM_RIGHT = "right arm"
  val ARM_LEFT = "left arm"
  val ARMS = "arms"
  val CHEST = "chest"
  val VITALS = "vitals"
  val ABDOMEN = "abdomen"
  val GROIN = "groin"
  val TORSO = "torso"
  val HANDS = "hands"
  val HAND_LEFT = "left hand"
  val HAND_RIGHT = "right hand"
  val FEET = "feet"
  val FOOT_RIGHT = "right foot"
  val FOOT_LEFT = "left foot"
  val SKIN = "skin"
  val BODY = "full body"
  val locMap: Map[String, Seq[String]] = Map(
    HEAD -> Seq(SKULL, FACE),
    LEGS -> Seq(LEG_RIGHT, LEG_LEFT),
    ARMS -> Seq(ARM_RIGHT, ARM_LEFT),
    TORSO -> Seq(CHEST, ABDOMEN, VITALS),
    HANDS -> Seq(HAND_RIGHT, HAND_LEFT),
    FEET -> Seq(FOOT_RIGHT, FOOT_LEFT),
    SKIN -> Seq(SKULL, FACE, NECK, ARM_LEFT, ARM_RIGHT, HAND_LEFT, HAND_RIGHT, CHEST, VITALS, ABDOMEN, GROIN,
      LEG_LEFT, LEG_RIGHT, FOOT_RIGHT, FOOT_LEFT),
    BODY -> Seq(SKULL, EYES, FACE, NECK, ARM_LEFT, ARM_RIGHT, HAND_RIGHT, HAND_LEFT, CHEST, VITALS, ABDOMEN,
      GROIN, LEG_LEFT, LEG_RIGHT, FOOT_LEFT, FOOT_RIGHT)) withDefault { x => Seq(x) }
  val woundCanBe = Set(EYES, SKULL, FACE, NECK, LEG_LEFT, LEG_RIGHT, ARM_LEFT, ARM_RIGHT, CHEST, VITALS, ABDOMEN, GROIN,
    HAND_LEFT, HAND_RIGHT, FOOT_LEFT, FOOT_RIGHT)
  val canBe = Set(EYES, SKULL, FACE, HEAD, NECK, LEG_LEFT, LEG_RIGHT, LEGS,
    ARM_LEFT, ARM_RIGHT, ARMS, CHEST, VITALS, ABDOMEN, GROIN, TORSO, HANDS, HAND_LEFT, HAND_RIGHT, FEET, FOOT_LEFT,
    FOOT_RIGHT, SKIN, BODY)
}

case class Item(
                 name: String = "",
                 var state: String = ItemState.STASH,
                 var dr: Int = 0,
                 var hp: Int = 1,
                 var hpLeft: Int = 1,
                 broken: Boolean = false,
                 var lc: Int = 5,
                 var tl: Int = 0,
                 notes: String = "",
                 var wt: Double = 0,
                 var cost: Double = 0,
                 var n: Int = 1,
                 var totalWt: Double = 0,
                 var totalCost: Double = 0)
  extends Possession {
  if (ItemState canBe state) () else state = ItemState.STASH
  if (dr < 0) dr = 0
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0
  if (n < 0) n = 0
  totalWt = wt * n
  totalCost = cost * n
}

case class FlaggedItem(ready: Boolean, data: Item) extends FlaggedFeature

object ItemState {
  val READY = "Ready"
  val EQUIPPED = "Equipped"
  val COMBAT = "Combat"
  val TRAVEL = "Travel"
  val STASH = "Stash"
  val canBe = Set(READY, EQUIPPED, COMBAT, TRAVEL, STASH)
}

case class DamageResistance(
                             skull: HitLocationDR = HitLocationDR(),
                             eyes: HitLocationDR = HitLocationDR(),
                             face: HitLocationDR = HitLocationDR(),
                             neck: HitLocationDR = HitLocationDR(),
                             armLeft: HitLocationDR = HitLocationDR(),
                             armRight: HitLocationDR = HitLocationDR(),
                             handLeft: HitLocationDR = HitLocationDR(),
                             handRight: HitLocationDR = HitLocationDR(),
                             chest: HitLocationDR = HitLocationDR(),
                             vitals: HitLocationDR = HitLocationDR(),
                             abdomen: HitLocationDR = HitLocationDR(),
                             groin: HitLocationDR = HitLocationDR(),
                             legLeft: HitLocationDR = HitLocationDR(),
                             legRight: HitLocationDR = HitLocationDR(),
                             footLeft: HitLocationDR = HitLocationDR(),
                             footRight: HitLocationDR = HitLocationDR())

case class HitLocationDR(front: DrSet = DrSet(), rear: DrSet = DrSet())

case class DrSet(var dr: Int = 0, var ep: Int = 0, var epi: Int = 0) {
  if (dr < 0) dr = 0
  if (ep < 0) ep = 0
  if (epi < 0) epi = 0

  def +(that: DrSet): DrSet = DrSet(this.dr + that.dr, this.ep + that.ep, this.epi + that.epi)

  def *(x: Int): DrSet = DrSet(this.dr * x, this.ep * x, this.epi * x)
}

case class Wound(
                  var location: String = HitLocation.CHEST,
                  var dType: String = DamageType.CRUSHING,
                  var points: Int = 1,
                  firstAid: Boolean = false,
                  bleeding: Boolean = false,
                  crippling: Boolean = false,
                  lasting: Boolean = false) {
  if (HitLocation woundCanBe location) () else location = HitLocation.CHEST
  if (DamageType canBe dType) () else dType = DamageType.CRUSHING
  if (points < 1) points = 1
}

case class Conditions(
                       unconscious: Boolean = false,
                       mortallyWounded: Boolean = false,
                       dead: Boolean = false,
                       var shock: Int = 0,
                       stunned: Boolean = false,
                       afflictions: Afflictions = Afflictions(),
                       var posture: String = Posture.STANDING,
                       closeCombat: Boolean = false,
                       grappled: Boolean = false,
                       pinned: Boolean = false,
                       sprinting: Boolean = false,
                       mounted: Boolean = false) {
  if (shock < 0) shock = 0 else if (shock > 8) shock = 8
  if (Posture canBe posture) () else posture = Posture.STANDING
}

case class Afflictions(
                        coughing: Boolean = false,
                        drowsy: Boolean = false,
                        drunk: Boolean = false,
                        euphoria: Boolean = false,
                        nauseated: Boolean = false,
                        pain: Boolean = false,
                        tipsy: Boolean = false,
                        agony: Boolean = false,
                        choking: Boolean = false,
                        daze: Boolean = false,
                        ecstasy: Boolean = false,
                        hallucinating: Boolean = false,
                        paralysis: Boolean = false,
                        retching: Boolean = false,
                        seizure: Boolean = false,
                        coma: Boolean = false,
                        heartAttack: Boolean = false)

object Posture {
  val STANDING = "Standing"
  val CROUCHING = "Crouching"
  val SITTING = "Sitting"
  val KNEELING = "Kneeling"
  val CRAWLING = "Crawling"
  val LYING_PRONE = "Prone"
  val LYING_FACE_UP = "On Back"
  val canBe = Set(STANDING, CROUCHING, SITTING, KNEELING, CRAWLING, LYING_PRONE, LYING_FACE_UP)
}

object Charlist {
  val rndUp: Double => Int = (x: Double) => math.ceil(x - 0.01).toInt
  implicit val afflictionsFormat: OFormat[Afflictions] = Json.format[Afflictions]
  implicit val conditionsFormat: OFormat[Conditions] = Json.format[Conditions]
  implicit val woundFormat: OFormat[Wound] = Json.format[Wound]
  implicit val itemFormat: OFormat[Item] = Json.format[Item]
  implicit val flaggedItemFormat: OFormat[FlaggedItem] = Json.format[FlaggedItem]
  implicit val drSetFormat: OFormat[DrSet] = Json.format[DrSet]
  implicit val armorComponentFormat: OFormat[ArmorComponent] = Json.format[ArmorComponent]
  implicit val armorElementFormat: OFormat[Armor] = Json.format[Armor]
  implicit val flaggedArmorFormat: OFormat[FlaggedArmor] = Json.format[FlaggedArmor]
  implicit val blockDefenceFormat: OFormat[BlockDefence] = Json.format[BlockDefence]
  implicit val rangedShotsFormat: OFormat[RangedShots] = Json.format[RangedShots]
  implicit val rangedRoFFormat: OFormat[RangedRoF] = Json.format[RangedRoF]
  implicit val rangedDamageFormat: OFormat[RangedDamage] = Json.format[RangedDamage]
  implicit val rangedAttackFormat: OFormat[RangedAttack] = Json.format[RangedAttack]
  implicit val meleeDamageFormat: OFormat[MeleeDamage] = Json.format[MeleeDamage]
  implicit val meleeAttackFormat: OFormat[MeleeAttack] = Json.format[MeleeAttack]
  implicit val weaponFormat: OFormat[Weapon] = Json.format[Weapon]
  implicit val flaggedWeaponFormat: OFormat[FlaggedWeapon] = Json.format[FlaggedWeapon]
  implicit val hitLocationFormat: OFormat[HitLocationDR] = Json.format[HitLocationDR]
  implicit val damageResistanceTotalFormat: OFormat[DamageResistance] = Json.format[DamageResistance]
  implicit val equipmentFormat: OFormat[Equipment] = Json.format[Equipment]
  implicit val bonusReactionFormat: OFormat[BonusReaction] = Json.format[BonusReaction]
  implicit val bonusAttributeCostFormat: OFormat[BonusAttributeCost] = Json.format[BonusAttributeCost]
  implicit val bonusDRFormat: OFormat[BonusDR] = Json.format[BonusDR]
  implicit val bonusDamageFormat: OFormat[BonusDamage] = Json.format[BonusDamage]
  implicit val bonusSkillFormat: OFormat[BonusSkill] = Json.format[BonusSkill]
  implicit val bonusAttributeFormat: OFormat[BonusAttribute] = Json.format[BonusAttribute]
  implicit val techniqueFormat: OFormat[Technique] = Json.format[Technique]
  implicit val flaggedTechniqueFormat: OFormat[FlaggedTechnique] = Json.format[FlaggedTechnique]
  implicit val skillFormat: OFormat[Skill] = Json.format[Skill]
  implicit val flaggedSkillFormat: OFormat[FlaggedSkill] = Json.format[FlaggedSkill]
  implicit val traitModifierFormat: OFormat[TraitModifier] = Json.format[TraitModifier]
  implicit val traitFormat: OFormat[Trait] = Json.format[Trait]
  implicit val flaggedTraitFormat: OFormat[FlaggedTrait] = Json.format[FlaggedTrait]
  implicit val reactionModFormat: OFormat[ReactionMod] = Json.format[ReactionMod]
  implicit val reactionFormat: OFormat[Reaction] = Json.format[Reaction]
  implicit val statIntFormat: OFormat[StatInt] = Json.format[StatInt]
  implicit val statDoubleFormat: OFormat[StatDouble] = Json.format[StatDouble]
  implicit val statsCurrentFormat: OFormat[StatsCurrent] = Json.format[StatsCurrent]
  implicit val statVarsFormat: OFormat[StatVars] = Json.format[StatVars]
  implicit val statsFormat: OFormat[Stats] = Json.format[Stats]
  implicit val descriptionFormat: OFormat[Description] = Json.format[Description]
  implicit val characterPointsFormat: OFormat[CharacterPoints] = Json.format[CharacterPoints]
  implicit val charlistFormat: OFormat[Charlist] = Json.format[Charlist]
}