package models.charlist

package object bonuses {

  case class DmgBns(perDie: Int = 0, min: Int = 0)

  object BonusToAttribute {
    val ST          = "st"
    val DX          = "dx"
    val IQ          = "iq"
    val HT          = "ht"
    val WILL        = "will"
    val FC          = "fright check"
    val PER         = "perception"
    val VISION      = "vision"
    val HEARING     = "hearing"
    val TASTE_SMELL = "taste smell"
    val TOUCH       = "touch"
    val DODGE       = "dodge"
    val PARRY       = "parry"
    val BLOCK       = "block"
    val BASIC_SPEED = "speed"
    val BASIC_MOVE  = "move"
    val HP          = "hp"
    val FP          = "fp"
    val SM          = "sm"
    val LIFT        = "lifting st"
    val STRIKE      = "striking st"

    val canBe = Set(ST, DX, IQ, HT, WILL, FC, PER, VISION, HEARING, TASTE_SMELL, TOUCH, DODGE, PARRY, BLOCK,
      BASIC_SPEED, BASIC_MOVE, HP, FP, SM, LIFT, STRIKE)
  }

  object NameCompare {
    val IS       = "is"
    val BEGINS   = "starts with"
    val CONTAINS = "contains"
    val ISNT     = "is not"

    val canBe    = Set(IS, BEGINS, CONTAINS)
    val spcCanBe = Set(IS, BEGINS, CONTAINS, ISNT)

    val fit: (String, Option[String], SkillBased) => Boolean = (s, spc, b) =>
      (b.skillCompare match {
        case IS       => s equalsIgnoreCase b.skill
        case BEGINS   => s startsWith b.skill.toLowerCase
        case CONTAINS => s contains b.skill.toLowerCase
        case _        => false
      }) && (b.spcCompare match {
        case None           => true
        case Some(IS)       => spc exists (b.spc exists _.equalsIgnoreCase)
        case Some(BEGINS)   => spc exists (s => b.spc exists (_.toLowerCase startsWith s))
        case Some(CONTAINS) => spc exists (s => b.spc exists (_.toLowerCase contains s))
        case Some(ISNT)     => !(spc exists (b.spc exists _.equalsIgnoreCase))
        case _              => false
      })
  }

}
