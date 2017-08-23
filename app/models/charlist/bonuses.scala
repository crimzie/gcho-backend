package models
package charlist

/** Parent trait uniting all case classes carrying a bonus to melee damage */
trait DamageBonusing {
  val dmgBonuses: Seq[BonusDamage]

  def on: Boolean = true

  def on_=(b: Boolean): Unit = ()

  def dmgBVal(s: => String, spc: => String, lvl: => Int): DmgBns =
    if (on) dmgBonuses.collect {
      case bd if NameCompare.fit(s, spc, bd) && bd.relSkill <= lvl =>
        if (bd.perDie) DmgBns(perDie = bd.bonus) else DmgBns(plain = bd.bonus)
    } reduce (_ + _)
    else DmgBns()
}
case class DmgBns(perDie: Int = 0, plain: Int = 0) {
  def +(that: DmgBns) = DmgBns(this.perDie + that.perDie, this.plain + that.plain)
}

case class BonusAttribute(var attr: String = BonusToAttribute.ST, perLvl: Boolean = false, bonus: Double = 0) {
  if (BonusToAttribute canBe attr) () else attr = BonusToAttribute.ST
}

case class BonusSkill(
                       skill: String = "",
                       var skillCompare: String = NameCompare.IS,
                       spc: String = "",
                       var spcCompare: String = NameCompare.ANY,
                       perLvl: Boolean = false,
                       bonus: Int = 0) extends SkillBased {
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
                        bonus: Int = 0) extends SkillBased {
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
  private val noteSum = (n: String) => Seq(notes, n) filter (_.nonEmpty) mkString "; "

  def +(that: BonusReaction): BonusReaction = copy(bonus = this.bonus + that.bonus, notes = noteSum(that.notes))

  def +~(that: BonusReaction): BonusReaction =
    copy(bonus = math.max(math.min(this.bonus + that.bonus, 4), -4), notes = noteSum(that.notes))
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
  val values: Map[Int, Double] = Map(ALLWAYS -> 1.0, OFTEN -> 2.0 / 3.0, SOMETIMES -> .5, OCCASIONALLY -> 1.0 / 3.0)
  val canBe = Set(ALLWAYS, OFTEN, SOMETIMES, OCCASIONALLY)
}
