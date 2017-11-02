package models.charlist

package object skill {

  object SkillDifficulty {
    val EASY      = "E"
    val AVERAGE   = "A"
    val HARD      = "H"
    val VERY_HARD = "VH"
    val WOW       = "W"
    // TODO: wildcard difficulty

    val values: Map[String, Int] = Map(EASY -> 0, AVERAGE -> -1, HARD -> -2, VERY_HARD -> -3, WOW -> -4)
    val canBe                    = Set(EASY, AVERAGE, HARD, VERY_HARD, WOW)
    val techniqueCanBe           = Set(AVERAGE, HARD)
  }

  object SkillBaseAttribute {
    val ST   = "ST"
    val IQ   = "IQ"
    val DX   = "DX"
    val HT   = "HT"
    val WILL = "Will"
    val PER  = "Per"

    val canBe = Set(ST, IQ, DX, HT, WILL, PER)
  }

}
