package models
package charlist

case class Conditions(
                       unconscious: Boolean = false,
                       mortallyWounded: Boolean = false,
                       dead: Boolean = false,
                       var shock: Int = 0,
                       stunned: Boolean = false,
                       afflictions: Afflictions = Afflictions(),
                       var posture: String = Posture.STANDING,
                       closeCombat: Boolean = false,
                       grappled: Boolean = false,
                       pinned: Boolean = false,
                       sprinting: Boolean = false,
                       mounted: Boolean = false) {
  if (shock < 0) shock = 0 else if (shock > 8) shock = 8
  if (Posture canBe posture) () else posture = Posture.STANDING
}

object Posture {
  val STANDING = "Standing"
  val CROUCHING = "Crouching"
  val SITTING = "Sitting"
  val KNEELING = "Kneeling"
  val CRAWLING = "Crawling"
  val LYING_PRONE = "Prone"
  val LYING_FACE_UP = "On Back"
  val canBe = Set(STANDING, CROUCHING, SITTING, KNEELING, CRAWLING, LYING_PRONE, LYING_FACE_UP)
}
