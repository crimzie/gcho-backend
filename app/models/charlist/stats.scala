package models
package charlist

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

/** Container for calculated secondary values. */
case class StatVars(
                     frightCheck: Int = 0,
                     vision: Int = 0,
                     hearing: Int = 0,
                     tasteSmell: Int = 0,
                     touch: Int = 0,
                     thrDmg: String = "",
                     swDmg: String = "",
                     bl: Int = 0,
                     combatEncumbrance: String = "",
                     travelEncumbrance: String = "",
                     combMove: Int = 0,
                     travMove: Int = 0,
                     dodge: Int = 0,
                     sm: Int = 0) {
  private val encLvl: Double => Int = {
    case d if d < 1.01 => 0
    case d if d < 2.01 => 1
    case d if d < 3.01 => 2
    case d if d < 6.01 => 3
    case d if d < 10.01 => 4
    case _ => 5
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

case class StatsCurrent(
                         var hpLost: Int = 0,
                         var fpLost: Int = 0,
                         reeling: Boolean = false,
                         tired: Boolean = false,
                         collapsing: Boolean = false,
                         fallingAslp: Boolean = false,
                         vision: Int = 0,
                         hearing: Int = 0,
                         move: Int = 0,
                         dodge: Int = 0) {
  if (hpLost < 0) hpLost = 0
  if (fpLost < 0) fpLost = 0

  def updated(hp: Int, fp: Int, mov: Int, ddg: Int): StatsCurrent = {
    val clps = hp <= hpLost
    val fslp = fp <= fpLost
    val m = if (clps || fslp) .5 else 1.0
    copy(
      reeling = hp * (2.0 / 3.0) < hpLost,
      tired = fp * (2.0 / 3.0) < fpLost,
      collapsing = clps,
      fallingAslp = fslp,
      move = Charlist rndUp (mov * m),
      dodge = Charlist rndUp (ddg * m))
  }
}

sealed abstract class Stat[A <: AnyVal, This](implicit x: scala.math.Numeric[A]) {

  import x._

  val delta: A
  val base: A
  val bonus: A
  val cpMod: Int
  val cp: Int

  def copy(delta: A = delta, base: A = base, bonus: A = bonus, cpMod: Int = cpMod, cp: Int = cp): This

  def value: A = delta + base + bonus

  def updated(base: A, bonus: A): This =
    copy(base = base, bonus = bonus, delta = if (lt(value, fromInt(0))) fromInt(0) - base - bonus else delta)

  def updatedCp(cost: Int): This =
    copy(cp = Charlist rndUp delta.toDouble * cost * math.max(.2, cpMod * .01))
}

case class StatInt(delta: Int = 0, base: Int = 0, bonus: Int = 0, cpMod: Int = 100, cp: Int = 0)
  extends Stat[Int, StatInt] {
  override def copy(
                     delta: Int = delta,
                     base: Int = base,
                     bonus: Int = bonus,
                     cpMod: Int = cpMod,
                     cp: Int = cp): StatInt =
    StatInt(delta, base, bonus, cpMod, cp)
}

case class StatDouble(delta: Double = 0, base: Double = 0, bonus: Double = 0, cpMod: Int = 100, cp: Int = 0)
  extends Stat[Double, StatDouble] {
  override def copy(
            delta: Double = delta,
            base: Double = base,
            bonus: Double = bonus,
            cpMod: Int = cpMod,
            cp: Int = cp): StatDouble = StatDouble(delta, base, bonus, cpMod, cp)
}
