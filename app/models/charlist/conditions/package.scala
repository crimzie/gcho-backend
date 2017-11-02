package models.charlist

package object conditions {

  object Shock {
    val canBe = Set(0, 1, 2, 3, 4, 6, 8)
  }

  object Posture {
    val STANDING      = "Standing"
    val CROUCHING     = "Crouching"
    val SITTING       = "Sitting"
    val KNEELING      = "Kneeling"
    val CRAWLING      = "Crawling"
    val LYING_PRONE   = "Prone"
    val LYING_FACE_UP = "On Back"

    val canBe = Set(STANDING, CROUCHING, SITTING, KNEELING, CRAWLING, LYING_PRONE, LYING_FACE_UP)
  }

}
