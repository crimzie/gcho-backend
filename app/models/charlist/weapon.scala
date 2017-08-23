package models
package charlist

import scala.language.postfixOps
import scala.util.Random

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
                   var totalCost: Double = 0) extends Possession {
  if (ItemState canBe state) () else state = ItemState.STASH
  if (bulk > 0) bulk = 0
  if (dr < 0) dr = 0
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0
  grips = attacksMelee.map(_.grip) ++ attacksRanged.map(_.grip) ++ blocks.map(_.grip) distinct;
  totalWt = wt + attacksRanged.map(_.shots.totalWt).sum
  totalCost = cost + attacksRanged.map(_.shots.totalCost).sum
}

case class FlaggedWeapon(
                          _id: String = Random.alphanumeric take 8 mkString,
                          ready: Boolean,
                          data: Weapon,
                          override var category: String = "",
                          override var name: String = "",
                          override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Weapon, FlaggedWeapon] {
  category = FlaggedFeature.WEAPONS
  name = data.name

  override def updated(user: String): FlaggedWeapon = copy(user = user)
}

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
                        dmgString: String = "") {
  if (AttackType canBe attackType) () else attackType = AttackType.WEAPON
  if (dmgDice < 0) dmgDice = 0
  if (ArmorDivisor canBe armorDiv) () else armorDiv = 1
  if (DamageType canBe dmgType) () else dmgType = DamageType.CRUSHING

  def withBns(thr: => (Int, Int), sw: => (Int, Int), bonus: DmgBns): MeleeDamage = {
    import AttackType._
    import DamageType._
    import Weapon._
    val (mDice: Int, mMod: Int) = attackType match {
      case THRUSTING => thr
      case SWINGING => sw
      case WEAPON => 0 -> 0
    }
    val dice: Int = dmgDice + mDice
    val mod = dmgMod + mMod + bonus.perDie * dice + bonus.plain
    val diceWMod: Int = dice + (if (mod > 0) mod / 3.5 else 0).toInt
    val modWDice: Int = if (mod > 0) (mod % 3.5).toInt else mod
    copy(dmgString = dmgType match {
      case SPECIAL => dmgType
      case AFFLICTION => s"HT${modStr(dmgMod)}"
      case _ =>
        val sp = if (attackType == WEAPON) "" else "+"
        s"$attackType$sp${diceWMod}d${modStr(modWDice)}${divStr(armorDiv)} $dmgType"
    })
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
  dmgString = {
    import Weapon._
    dmgType match {
      case DamageType.SPECIAL => dmgType
      case DamageType.AFFLICTION => s"HT${modStr(dmgMod)}"
      case _ =>
        val fragStr = if (fragDice > 0) " [" + fragDice + "d]" else ""
        s"${dmgDice}d${multStr(diceMult)}${modStr(dmgMod)}${divStr(armorDiv)} $dmgType$fragStr"
    }
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
  rofString = s"$rof${Weapon multStr rofMult}${if (rof != 1 && rofFA) "!" else ""}"
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
  private val shotsStr: String = if (shots != 0) "" + shotsLoaded + "/" + shots else ""
  shotsString = s"$shotsStr$reload${if (shotsCarried != 0) " " + shotsCarried else ""}"
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

object Weapon {
  val modStr: Int => String = m => if (m > 0) s"+$m" else if (m < 0) s"$m" else ""
  val multStr: Int => String = m => if (m != 1) "x" + m else ""
  val divStr: Double => String = d => if (d < 1) "(" + d + ")" else if (d > 1) "(" + d.toInt + ")" else ""
}
