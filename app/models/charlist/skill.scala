package models
package charlist

import scala.language.postfixOps
import scala.util.Random

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
                  bonus: Int = 0,
                  categories: Seq[String] = Nil,
                  notes: String = "",
                  var cp: Int = 1,
                  var relLvl: Int = 0,
                  lvl: Int = 0)
  extends DamageBonusing {
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (SkillBaseAttribute canBe attr) () else attr = SkillBaseAttribute.DX
  if (SkillDifficulty canBe diff) () else diff = SkillDifficulty.EASY
  skillString = name + (if (tl != 0) s"/TL$tl" else "") + (if (spc != "") s" ($spc)" else "")
  if (relLvl < SkillDifficulty.values(diff)) relLvl = SkillDifficulty values diff
  private val l = relLvl - (SkillDifficulty values diff) + 1
  cp = l * 4 - (if (l > 2) 8 else if (l > 1) 6 else 3)

  def updated(bonus: Int, attrVal: Int, enc: => Int): Skill = copy(
    bonus = bonus,
    lvl = attrVal + relLvl - (if (encumbr) enc else 0) + bonus)
}

case class FlaggedSkill(
                         _id: String = Random.alphanumeric take 8 mkString,
                         ready: Boolean,
                         data: Skill,
                         override var category: String = "",
                         override var name: String = "",
                         override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Skill, FlaggedSkill] {
  category = FlaggedFeature.SKILLS
  name = data.skillString

  override def updated(user: String): FlaggedSkill = copy(user = user)
}

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
                      lvl: Int = 0) {
  if (SkillDifficulty techniqueCanBe diff) () else diff = SkillDifficulty.AVERAGE
  if (maxLvl <= defLvl) maxLvl = defLvl + 1
  if (relLvl < defLvl) relLvl = defLvl else if (relLvl > maxLvl) relLvl = maxLvl
  tchString = s"$name ($skill${if (spc != "") " (" + spc + ")" else ""})"
  cp = relLvl - defLvl + (if (diff == SkillDifficulty.HARD && relLvl > defLvl) 1 else 0)

  def updated(skill: Int): Technique = this.copy(lvl = skill + relLvl)
}

case class FlaggedTechnique(
                             _id: String = Random.alphanumeric take 8 mkString,
                             ready: Boolean,
                             data: Technique,
                             override var category: String = "",
                             override var name: String = "",
                             override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Technique, FlaggedTechnique] {
  category = FlaggedFeature.TECHNIQUES
  name = data.tchString

  override def updated(user: String): FlaggedTechnique = copy(user = user)
}

trait SkillBased {
  val skill: String
  var skillCompare: String
  val spc: String
  var spcCompare: String
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
