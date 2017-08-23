package models
package charlist

import org.joda.time.DateTime
import play.api.libs.json.{Json, OFormat}

import scala.collection.breakOut
import scala.language.postfixOps
import scala.util.Random

/**
  * Case class for conversion between JSON input and database entries and for calculation and validation of
  * charlist's inner secondary values on construction. Initialization automatically recalculates input values, producing
  * its instance with correct data.
  */
case class Charlist(
                     _id: String = Random.alphanumeric take 8 mkString,
                     timestamp: Long = DateTime.now.getMillis,
                     player: String = "", // TODO: user auth implementation
                     access: Seq[String] = Nil, // For future functionality
                     name: String = "New Character",
                     var cp: CharacterPoints = CharacterPoints(),
                     description: Description = Description(),
                     var stats: Stats = Stats(),
                     var statVars: StatVars = StatVars(),
                     var damageResistance: DamageResistance = DamageResistance(),
                     var reactions: Seq[Reaction] = Nil,
                     traits: Seq[Trait] = Trait(
                       name = "Human",
                       category = TraitCategory.RACE,
                       modifiers =
                         TraitModifier(drBonuses = BonusDR(HitLocation.SKULL :: Nil, protection = DrSet(2)) :: Nil) ::
                           Nil) :: Nil,
                     var skills: Seq[Skill] = Nil,
                     var techniques: Seq[Technique] = Nil,
                     var equip: Equipment = Equipment(),
                     var currentStats: StatsCurrent = StatsCurrent(),
                     wounds: Seq[Wound] = Nil,
                     conditions: Conditions = Conditions(),
                     var api: String = "") {
  api = Charlist.VERSION_VAL

  {
    import BonusToAttribute._
    import HitLocation._

    val traitLocs: Seq[(String, DrSet, DrSet)] = for {
      b <- traits flatMap (_.drBonuses)
      l <- b.locations flatMap locMap
    } yield (l, if (b.front) b.protection else DrSet(), if (b.back) b.protection else DrSet())
    val equipLocs: Seq[(String, DrSet, DrSet)] = for {
      c <- equip.armor flatMap (_.components)
      l <- c.locations flatMap locMap
    } yield (l, if (c.front) c.protection else DrSet(), if (c.back) c.protection else DrSet())
    val drMap: Map[String, HitLocationDR] = traitLocs ++ equipLocs groupBy {
      case (location, _, _) => location
    } mapValues {
      tupl3s => HitLocationDR((DrSet() /: tupl3s) (_ + _._2), (DrSet() /: tupl3s) (_ + _._3))
    } withDefaultValue HitLocationDR(DrSet(), DrSet())
    damageResistance = DamageResistance(skull = drMap(SKULL), eyes = drMap(EYES), face = drMap(FACE),
      neck = drMap(NECK), armLeft = drMap(ARM_LEFT), armRight = drMap(ARM_RIGHT), handLeft = drMap(HAND_LEFT),
      handRight = drMap(HAND_RIGHT), chest = drMap(CHEST), vitals = drMap(VITALS), abdomen = drMap(ABDOMEN),
      groin = drMap(GROIN), legLeft = drMap(LEG_LEFT), legRight = drMap(LEG_RIGHT), footLeft = drMap(FOOT_LEFT),
      footRight = drMap(FOOT_RIGHT))

    val bonuses: Map[String, Double] = traits flatMap (_.attrBonusValues) groupBy (_.attr) mapValues {
      seq => (0.0 /: seq) (_ + _.bonus)
    } withDefaultValue 0.0
    val st = stats.st.updated(10, bonuses(ST).toInt)
    val dx = stats.dx.updated(10, bonuses(DX).toInt)
    val ht = stats.ht.updated(10, bonuses(HT).toInt)
    val bs = stats.basicSpeed.updated((dx.value + ht.value) * .25, bonuses(BASIC_SPEED))
    stats = stats.copy(
      st = st,
      dx = dx,
      iq = stats.iq.updated(10, bonuses(IQ).toInt),
      ht = ht,
      will = stats.will.updated(10, bonuses(WILL).toInt),
      per = stats.per.updated(10, bonuses(PER).toInt),
      basicSpeed = bs,
      basicMove = stats.basicMove.updated(bs.value.toInt, bonuses(BASIC_MOVE).toInt),
      hp = stats.hp.updated(st.value, bonuses(HP).toInt),
      fp = stats.fp.updated(ht.value, bonuses(FP).toInt),
      liftSt = stats.liftSt.updated(st.value, bonuses(LIFT).toInt),
      strikeSt = stats.strikeSt.updated(st.value, bonuses(STRIKE).toInt))
    statVars = statVars
      .copy(
        sm = bonuses(SM).toInt,
        frightCheck = math.min(13, stats.will.value + bonuses(FC).toInt),
        vision = stats.per.value + bonuses(VISION).toInt,
        hearing = stats.per.value + bonuses(HEARING).toInt,
        tasteSmell = stats.per.value + bonuses(TASTE_SMELL).toInt,
        touch = stats.per.value + bonuses(TOUCH).toInt,
        bl = stats.liftSt.value * stats.liftSt.value * .2 toInt)
      .updated(
        equip.totalCombWt,
        equip.totalTravWt,
        stats.basicMove.value,
        stats.basicSpeed.value.toInt + 3 + bonuses(DODGE).toInt)
    currentStats = currentStats.updated(stats.hp.value, stats.fp.value, statVars.combMove, statVars.dodge)
  }

  {
    import SkillBaseAttribute._

    val attrVal: Map[String, Int] = Map(
      ST -> stats.st.value,
      DX -> stats.dx.value,
      IQ -> stats.iq.value,
      HT -> stats.ht.value,
      WILL -> stats.will.value,
      PER -> stats.per.value)
    skills = for {s <- skills} yield
      s.updated((0 /: traits) (_ + _.skillBonusValue(s.name, s.spc)), attrVal(s.attr), statVars cEnc equip.totalCombWt)
    val sklCache = collection.mutable.Map[(String, String), Int]()
    techniques = for {t <- techniques} yield {
      lazy val lvl = skills collectFirst { case sk if sk.name == t.skill && sk.spc == t.spc => sk.lvl } getOrElse 0
      t updated sklCache.getOrElseUpdate(t.skill -> t.spc, lvl)
    }

    reactions = (for {
      (affected, seq) <- traits.flatMap(_.reactBonuses) ++ skills.flatMap(_.reactBonuses) groupBy (_.affected)
    } yield {
      val mods = (for {(freq, seq2) <- seq groupBy (_.freq)} yield {
        val reactions = for {(reputation, seq3) <- seq2 groupBy (_.reputation)}
          yield if (reputation) seq3 reduce (_ +~ _) else seq3 reduce (_ + _)
        val bonusReaction = reactions reduce (_ + _)
        ReactionMod(freq, bonusReaction.bonus, bonusReaction.notes)
      }) (breakOut)
      Reaction(affected, mods)
    }) (breakOut) // TODO: make freq-bonus sums

    val (thrD: Int, thrM: Int) = stats.strikeSt.value match {
      case x if x < 1 => (0, 0)
      case x if x < 11 => (1, (x - 1) / 2 - 6)
      case x => ((x - 3) / 8, (x - 3) / 2 % 4 - 1)
    }
    val (swD: Int, swM: Int) = stats.strikeSt.value match {
      case x if x < 1 => (0, 0)
      case x if x < 9 => (1, (x - 1) / 2 - 5)
      case x => ((x - 5) / 4, (x - 5) % 4 - 1)
    }
    val dStr: (Int, Int) => String = (d, m) => s"${d}d${if (m < 0) m else if (m > 0) "+" + m else ""}"
    statVars = statVars.copy(thrDmg = dStr(thrD, thrM), swDmg = dStr(swD, swM))
    val bnsDmgCache = collection.mutable.Map[(String, String), DmgBns]()
    equip = equip.copy(weapons = for {w <- equip.weapons} yield {
      val attacks = for {ma@MeleeAttack(_, _, _, dmg, flw, lnk, s, sp, _, _, _, _, _, _, _) <- w.attacksMelee}
        yield {
          lazy val bnsUpd = skills collectFirst { case sk if sk.name == s && sk.spc == sp => sk.relLvl } map {
            l => (DmgBns() /: (skills ++ traits.withFilter(_.active).flatMap(_.modifiers))) (_ + _.dmgBVal(s, sp, l))
          } getOrElse DmgBns()
          ma.copy(
            damage = dmg.withBns(thrD -> thrM, swD -> swM, bnsDmgCache getOrElseUpdate(s -> sp, bnsUpd)),
            followup = for {m <- flw} yield m.withBns(thrD -> thrM, swD -> swM, DmgBns()),
            linked = for {m <- lnk} yield m.withBns(thrD -> thrM, swD -> swM, DmgBns()))
        }
      w.copy(attacksMelee = attacks)
    })

    val costMods: Map[String, Int] = traits flatMap (_.attrCostMods) groupBy (_.attr) mapValues {
      seq => (0 /: seq) (_ + _.cost)
    } withDefaultValue 0
    val st: StatInt = stats.st.copy(cpMod = 100 + costMods(ST))
    val dx: StatInt = stats.dx.copy(cpMod = 100 + costMods(DX))
    val iq: StatInt = stats.iq.copy(cpMod = 100 + costMods(IQ))
    val ht: StatInt = stats.ht.copy(cpMod = 100 + costMods(HT))
    stats = stats.copy(
      st = st.updatedCp(10),
      dx = dx.updatedCp(20),
      iq = iq.updatedCp(20),
      ht = ht.updatedCp(10),
      will = stats.will.updatedCp(5),
      per = stats.per.updatedCp(5),
      liftSt = stats.liftSt.updatedCp(3),
      strikeSt = stats.strikeSt.updatedCp(5),
      hp = stats.hp.updatedCp(2),
      fp = stats.fp.updatedCp(3),
      basicSpeed = stats.basicSpeed.updatedCp(20),
      basicMove = stats.basicMove.updatedCp(5))
    val (aCp: Seq[Int], dCp: Seq[Int]) = traits map (_.cp) partition (_ > 0)
    cp = cp.copy(
      stats = stats.cp,
      adv = aCp.sum,
      dis = dCp.sum,
      skills = skills.map(_.cp).sum + techniques.map(_.cp).sum,
      unspent = cp.cp - cp.skills - cp.stats - cp.adv - cp.dis)
  }
}

case class CharacterPoints(
                            cp: Int = 0,
                            stats: Int = 0,
                            adv: Int = 0,
                            dis: Int = 0,
                            skills: Int = 0,
                            unspent: Int = 0)

case class Description(
                        age: String = "",
                        height: String = "",
                        weight: String = "",
                        bio: String = "")

object Charlist {
  val ID: String = "_id"
  val TIMESTAMP: String = "timestamp"
  val PLAYER: String = "player"
  val NAME: String = "name"
  val VERSION_VAL: String = "0.3.4"
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
