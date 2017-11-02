package models.charlist
package bonuses

trait HasBonusDamage {
  val dmgBonuses: Seq[DamageBonus]

  def on: Boolean

  def dmgBVal(s: => String, spc: => Option[String], relLvl: => Int): Seq[DmgBns] = {
    lazy val lcs = s.toLowerCase
    lazy val lcspc = spc.map(_.toLowerCase)
    if (on) dmgBonuses collect {
      case bd if NameCompare.fit(lcs, lcspc, bd) && !bd.minRelSkill.exists(_ > relLvl) =>
        DmgBns(bd.perDie getOrElse 0, bd.minBonus)
    } else Nil
  }
}
