package models.charlist

package object reaction {

  object ReactionFrequency {
    val ALWAYS       = 16
    val OFTEN        = 13
    val SOMETIMES    = 10
    val OCCASIONALLY = 7

    val values: Map[Int, Double] = Map(ALWAYS -> 1.0, OFTEN -> 2.0 / 3.0, SOMETIMES -> .5, OCCASIONALLY -> 1.0 / 3.0)
    val canBe                    = Set(ALWAYS, OFTEN, SOMETIMES, OCCASIONALLY)
  }

}
