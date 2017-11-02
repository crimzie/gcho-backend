package models
package charlist

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.`trait`.{Trait, TraitCategory, TraitModifier, TraitModifierCategory}
import models.charlist.armor._
import models.charlist.bonuses._
import models.charlist.conditions.Conditions
import models.charlist.dr.{DamageResistance, DrSet, HitLocationDR}
import models.charlist.equipment.Equipment
import models.charlist.reaction.{Reaction, ReactionMod}
import models.charlist.skill._
import models.charlist.stats._
import models.charlist.weapon._
import org.joda.time.DateTime
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.collection.{breakOut, mutable}
import scala.language.postfixOps
import scala.util.Random

/**
  * Case class for conversion between JSON input and database entries and for calculation and validation of
  * charlist's inner secondary values on construction. Initialization automatically recalculates input values, producing
  * its instance with correct data.
  */
case class Charlist(
    @ApiModelProperty(required = false)
    _id: String = Charlist.randomId(),
    @ApiModelProperty(required = false)
    timestamp: Long = DateTime.now getMillis,
    @ApiModelProperty(required = false)
    player: String,
    @ApiModelProperty(required = false)
    access: Seq[String] = Nil,
    name: String = "New Character",
    cp: CharacterPoints = Charlist.defCharacterPoints,
    @ApiModelProperty(required = false)
    description: Description = Charlist.defDescription,
    @ApiModelProperty(required = false)
    stats: Stats = Charlist.defStats,
    @ApiModelProperty(required = false)
    statVars: StatVars = Charlist.defStatVars,
    @ApiModelProperty(required = false, value = "Calculated value.")
    dr: DamageResistance = Charlist.defDamageResistance,
    @ApiModelProperty(required = false, value = "Calculated value.")
    reactions: Seq[Reaction] = Nil,
    @ApiModelProperty(required = false)
    traits: Seq[Trait] = Charlist.traitHuman :: Nil,
    @ApiModelProperty(required = false)
    skills: Seq[Skill] = Nil,
    @ApiModelProperty(required = false)
    techniques: Seq[Technique] = Nil,
    @ApiModelProperty(required = false)
    equip: Equipment = Charlist.defEquipment,
    @ApiModelProperty(required = false)
    currentStats: StatsCurrent = Charlist.defStatsCurrent,
    @ApiModelProperty(required = false)
    wounds: Seq[Wound] = Nil,
    @ApiModelProperty(required = false)
    conditions: Conditions = Charlist.defConditions,
    @ApiModelProperty(required = false, value = "Generated string.")
    api: String = Charlist.VERSION_VAL) {
  def calc(): Charlist = {
    import BonusToAttribute._
    import HitLocation._

    val traitLocs: Seq[(String, DrSet, DrSet)] = for {
      b <- traits flatMap (_.drBonuses)
      l <- b.locations flatMap locMap
    } yield (l, if (b.front) b.protection else Charlist.defDrSet, if (b.back) b.protection else Charlist.defDrSet)
    val equipLocs: Seq[(String, DrSet, DrSet)] = for {
      c <- equip.armor flatMap (_.components)
      l <- c.locations flatMap locMap
    } yield (l, if (c.front) c.protection else Charlist.defDrSet, if (c.back) c.protection else Charlist.defDrSet)
    val drMap: Map[String, HitLocationDR] = traitLocs ++ equipLocs groupBy {
      case (location, _, _) => location
    } mapValues {
      tupl3s => HitLocationDR((Charlist.defDrSet /: tupl3s) (_ + _._2), (Charlist.defDrSet /: tupl3s) (_ + _._3))
    } withDefaultValue Charlist.defHitLocDR
    val bonuses: Map[String, Double] = traits flatMap (_.attrBonusValues) groupBy (_.attr) mapValues {
      seq => (0.0 /: seq) (_ + _.bonus)
    } withDefaultValue 0.0
    val costMods: Map[String, Int] = traits flatMap (_.attrCostMods) groupBy (_.attr) mapValues {
      seq => (0 /: seq) (_ + _.cost)
    } withDefaultValue 0
    val stˆ = stats.st calc(10, bonuses(ST).toInt, 10, costMods(SkillBaseAttribute.ST))
    val htˆ = stats.ht calc(10, bonuses(HT).toInt, 10, costMods(SkillBaseAttribute.HT))
    val dxˆ = stats.dx calc(10, bonuses(DX).toInt, 20, costMods(SkillBaseAttribute.DX))
    val speedˆ = stats.basicSpeed calc((dxˆ.value + htˆ.value) * .25, bonuses(BASIC_SPEED).toInt, 20)
    val statsˆ = Stats(
      stˆ,
      dxˆ,
      stats.iq calc(10, bonuses(IQ).toInt, 20, costMods(SkillBaseAttribute.IQ)),
      htˆ,
      stats.will calc(10, bonuses(WILL).toInt, 5),
      stats.per calc(10, bonuses(PER).toInt, 5),
      stats.liftSt calc(stˆ.value, bonuses(LIFT).toInt, 3),
      stats.strikeSt calc(stˆ.value, bonuses(STRIKE).toInt, 5),
      stats.hp calc(stˆ.value, bonuses(HP).toInt, 2),
      stats.fp calc(htˆ.value, bonuses(FP).toInt, 3),
      speedˆ,
      stats.basicMove calc(speedˆ.value.toInt, bonuses(BASIC_MOVE).toInt, 5))

    val (thrD: Int, thrM: Int) = statsˆ.strikeSt.value match {
      case x if x < 1  => (0, 0)
      case x if x < 11 => (1, (x - 1) / 2 - 6)
      case x           => ((x - 3) / 8, (x - 3) / 2 % 4 - 1)
    }
    val (swD: Int, swM: Int) = statsˆ.strikeSt.value match {
      case x if x < 1 => (0, 0)
      case x if x < 9 => (1, (x - 1) / 2 - 5)
      case x          => ((x - 5) / 4, (x - 5) % 4 - 1)
    }
    val dStr: (Int, Int) => String = (d, m) => s"${d}d${if (m < 0) m else if (m > 0) "+" + m else ""}"
    val (aCp: Seq[Int], dCp: Seq[Int]) = traits map (_.cp) partition (_ > 0)

    val statVarsˆ = statVars
      .copy(
        sm = bonuses(SM).toInt,
        frightCheck = math.min(13, statsˆ.will.value + bonuses(FC).toInt),
        vision = statsˆ.per.value + bonuses(VISION).toInt,
        hearing = statsˆ.per.value + bonuses(HEARING).toInt,
        tasteSmell = statsˆ.per.value + bonuses(TASTE_SMELL).toInt,
        touch = statsˆ.per.value + bonuses(TOUCH).toInt,
        bl = statsˆ.liftSt.value * statsˆ.liftSt.value * .2 toInt)
      .updated(
        equip.totalCombWt,
        equip.totalTravWt,
        statsˆ.basicMove.value,
        speedˆ.value.toInt + 3 + bonuses(DODGE).toInt)
    val skillsˆ = for {s <- skills} yield s.calc(
      (0 /: traits) (_ + _.skillBonusValue(s.name, s.spc)),
      statsˆ.byName(s.attr),
      statVarsˆ cEnc equip.totalCombWt)

    val sklCache = mutable.Map[(String, Option[String]), Int]()
    val bnsDmgCache = mutable.Map[(String, Option[String]), Seq[DmgBns]]()

    val advCp = aCp.sum
    val disCp = dCp.sum
    val skillsCp = skillsˆ.map(_.cp).sum + techniques.map(_.cp).sum

    copy(
      dr = DamageResistance(skull = drMap(SKULL), eyes = drMap(EYES), face = drMap(FACE),
        neck = drMap(NECK), armLeft = drMap(ARM_LEFT), armRight = drMap(ARM_RIGHT), handLeft = drMap(HAND_LEFT),
        handRight = drMap(HAND_RIGHT), chest = drMap(CHEST), vitals = drMap(VITALS), abdomen = drMap(ABDOMEN),
        groin = drMap(GROIN), legLeft = drMap(LEG_LEFT), legRight = drMap(LEG_RIGHT), footLeft = drMap(FOOT_LEFT),
        footRight = drMap(FOOT_RIGHT)),
      stats = statsˆ,
      statVars = statVarsˆ.copy(thrDmg = dStr(thrD, thrM), swDmg = dStr(swD, swM)),
      currentStats = currentStats.updated(statsˆ.hp.value, statsˆ.fp.value, statVarsˆ.combMove, statVarsˆ.dodge),
      skills = skillsˆ,
      techniques = for {t <- techniques} yield {
        t updated sklCache.getOrElseUpdate(
          t.skill -> t.spc,
          skillsˆ.collectFirst { case sk if sk.name == t.skill && sk.spc == t.spc => sk.lvl } getOrElse 0)
      },
      reactions = (for {
        (affected, seq) <- traits.flatMap(_.reactBonuses) ++ skillsˆ.flatMap(_.reactBonuses) groupBy (_.affected)
      } yield {
        val mods = (for {(freq, seq2) <- seq groupBy (_.freq)} yield {
          val reactions = for {(reputation, seq3) <- seq2 groupBy (_.reputation)}
            yield if (reputation) seq3 reduce (_ +~ _) else seq3 reduce (_ + _)
          val bonusReaction = reactions reduce (_ + _)
          ReactionMod(freq, bonusReaction.bonus, bonusReaction.notes)
        }) (breakOut)
        Reaction(affected, mods)
      }) (breakOut), // TODO: make freq-bonus sums
      equip = equip.copy(weapons = for {w <- equip.weapons} yield {
        val attacks = for {ma@MeleeAttack(_, _, _, dmg, _, _, s, sp, _, _, _, _, _, _) <- w.attacksMelee} yield {
          lazy val bnsUpd: Seq[DmgBns] = skillsˆ collectFirst { case sk if sk.name == s && sk.spc == sp =>
            skillsˆ ++ traits.withFilter(_.active).flatMap(_.modifiers) flatMap (_.dmgBVal(s, sp, sk.relLvl))
          } getOrElse Nil
          ma.copy(damage = dmg.calc(thrD -> thrM, swD -> swM, bnsDmgCache getOrElseUpdate(s -> sp, bnsUpd)))
        }
        w.copy(attacksMelee = attacks)
      }),
      cp = cp.copy(
        stats = statsˆ.cp,
        adv = advCp,
        dis = disCp,
        skills = skillsCp,
        unspent = cp.cp - statsˆ.cp - advCp - disCp - skillsCp),
      api = Charlist.VERSION_VAL)
  }
}

object Charlist {
  def randomId(): String = Random.alphanumeric take 8 mkString

  val ID         : String = NameOf.nameOf[Charlist](_._id)
  val TIMESTAMP  : String = NameOf.nameOf[Charlist](_.timestamp)
  val PLAYER     : String = NameOf.nameOf[Charlist](_.player)
  val NAME       : String = NameOf.nameOf[Charlist](_.name)
  val VERSION_VAL: String = "0.3.5"

  val rndUp: Double => Int = (x: Double) => math.ceil(x - 0.01).toInt
  private  val skullTraitModifier             = TraitModifier(
    name = "Skull",
    cat = TraitModifierCategory.DEFAULT,
    drBonuses = DRBonus(HitLocation.SKULL :: Nil, protection = DrSet(2)) :: Nil)
  private  val traitHuman                     =
    Trait(name = "Human", category = TraitCategory.RACE, modifiers = skullTraitModifier :: Nil)
  lazy     val defCharacterPoints             = CharacterPoints()
  lazy     val defDescription                 = Description()
  lazy     val defStats                       = Stats()
  lazy     val defStatVars                    = StatVars()
  lazy     val defDamageResistance            = DamageResistance()
  lazy     val defEquipment                   = Equipment()
  lazy     val defStatsCurrent                = StatsCurrent()
  lazy     val defConditions                  = Conditions()
  lazy     val defDrSet                       = DrSet()
  lazy     val defHitLocDR                    = HitLocationDR(defDrSet, defDrSet)
  lazy     val defCharlist: Charlist          = Charlist(player = "").calc()
  private  val reads      : Reads[Charlist]   = (
    (JsPath \ ID).readWithDefault(randomId()) ~
      (JsPath \ TIMESTAMP).readWithDefault(DateTime.now getMillis) ~
      (JsPath \ PLAYER).readWithDefault("") ~
      (JsPath \ NameOf.nameOf[Charlist](_.access)).readWithDefault[Seq[String]](Nil) ~
      (JsPath \ NAME).read[String] ~
      (JsPath \ NameOf.nameOf[Charlist](_.cp)).read[CharacterPoints] ~
      (JsPath \ NameOf.nameOf[Charlist](_.description)).readWithDefault(defDescription) ~
      (JsPath \ NameOf.nameOf[Charlist](_.stats)).readWithDefault(defStats) ~
      (JsPath \ NameOf.nameOf[Charlist](_.statVars)).readWithDefault(defStatVars) ~
      Reads.pure(defDamageResistance) ~
      Reads.pure[Seq[Reaction]](Nil) ~
      (JsPath \ NameOf.nameOf[Charlist](_.traits)).readWithDefault[Seq[Trait]](traitHuman :: Nil) ~
      (JsPath \ NameOf.nameOf[Charlist](_.skills)).readWithDefault[Seq[Skill]](Nil) ~
      (JsPath \ NameOf.nameOf[Charlist](_.techniques)).readWithDefault[Seq[Technique]](Nil) ~
      (JsPath \ NameOf.nameOf[Charlist](_.equip)).readWithDefault(defEquipment) ~
      (JsPath \ NameOf.nameOf[Charlist](_.currentStats)).readWithDefault(defStatsCurrent) ~
      (JsPath \ NameOf.nameOf[Charlist](_.wounds)).readWithDefault[Seq[Wound]](Nil) ~
      (JsPath \ NameOf.nameOf[Charlist](_.conditions)).readWithDefault(defConditions) ~
      Reads.pure(VERSION_VAL)) (Charlist.apply _)
  implicit val format     : OFormat[Charlist] = OFormat(reads, Json.writes[Charlist])
}
