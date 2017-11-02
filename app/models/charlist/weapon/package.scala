package models.charlist

package object weapon {
  val modStr : Int => String    = m => if (m > 0) s"+$m" else if (m < 0) s"$m" else ""
  val multStr: Int => String    = m => if (m != 1) "x" + m else ""
  val divStr : Double => String = d => if (d < 1) "(" + d + ")" else if (d > 1) "(" + d.toInt + ")" else ""

  object AttackType {
    val THRUSTING = "thr"
    val SWINGING  = "sw"

    val canBe = Set(THRUSTING, SWINGING)
  }

  object ArmorDivisor {
    val canBe = Set(0.1, 0.2, 0.5, 1, 2, 3, 5, 10, 100)
  }

  object DamageType {
    val CRUSHING           = "cr"
    val CRUSHING_EXPLOSION = "cr ex"
    val CUTTING            = "cut"
    val IMPALING           = "imp"
    val PIERCING_SMALL     = "pi-"
    val PIERCING           = "pi"
    val PIERCING_LARGE     = "pi+"
    val PIERCING_HUGE      = "pi++"
    val BURNING            = "burn"
    val BURNING_EXPLOSION  = "burn ex"
    val TOXIC              = "tox"
    val CORROSION          = "cor"
    val AFFLICTION         = "aff"
    val FATIGUE            = "fat"
    val SPECIAL            = "spec."

    val canBe = Set(CRUSHING, CRUSHING_EXPLOSION, CUTTING, IMPALING, PIERCING_SMALL, PIERCING, PIERCING_LARGE,
      PIERCING_HUGE, BURNING, BURNING_EXPLOSION, TOXIC, CORROSION, AFFLICTION, FATIGUE, SPECIAL)
  }

}
