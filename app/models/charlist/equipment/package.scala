package models.charlist

package object equipment {

  object ItemState {
    val READY    = "Ready"
    val EQUIPPED = "Equipped"
    val COMBAT   = "Combat"
    val TRAVEL   = "Travel"
    val STASH    = "Stash"

    val canBe = Set(READY, EQUIPPED, COMBAT, TRAVEL, STASH)
  }

}
