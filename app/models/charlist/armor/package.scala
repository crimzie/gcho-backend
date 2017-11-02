package models.charlist

package object armor {

  object DrType {
    val HARD  = "hard"
    val SOFT  = "soft"
    val FIELD = "force field"
    val SKIN  = "tough skin"
    val canBe = Set(HARD, SOFT, FIELD, SKIN)
  }

  object HitLocation {
    val EYES       = "eyes"
    val SKULL      = "skull"
    val FACE       = "face"
    val HEAD       = "head"
    val NECK       = "neck"
    val LEG_RIGHT  = "right leg"
    val LEG_LEFT   = "left leg"
    val LEGS       = "legs"
    val ARM_RIGHT  = "right arm"
    val ARM_LEFT   = "left arm"
    val ARMS       = "arms"
    val CHEST      = "chest"
    val VITALS     = "vitals"
    val ABDOMEN    = "abdomen"
    val GROIN      = "groin"
    val TORSO      = "torso"
    val HANDS      = "hands"
    val HAND_LEFT  = "left hand"
    val HAND_RIGHT = "right hand"
    val FEET       = "feet"
    val FOOT_RIGHT = "right foot"
    val FOOT_LEFT  = "left foot"
    val SKIN       = "skin"
    val BODY       = "full body"

    val locMap: Map[String, Seq[String]] = Map(
      HEAD -> Seq(SKULL, FACE),
      LEGS -> Seq(LEG_RIGHT, LEG_LEFT),
      ARMS -> Seq(ARM_RIGHT, ARM_LEFT),
      TORSO -> Seq(CHEST, ABDOMEN, VITALS),
      HANDS -> Seq(HAND_RIGHT, HAND_LEFT),
      FEET -> Seq(FOOT_RIGHT, FOOT_LEFT),
      SKIN -> Seq(SKULL, FACE, NECK, ARM_LEFT, ARM_RIGHT, HAND_LEFT, HAND_RIGHT, CHEST, VITALS, ABDOMEN, GROIN,
        LEG_LEFT, LEG_RIGHT, FOOT_RIGHT, FOOT_LEFT),
      BODY -> Seq(SKULL, EYES, FACE, NECK, ARM_LEFT, ARM_RIGHT, HAND_RIGHT, HAND_LEFT, CHEST, VITALS, ABDOMEN,
        GROIN, LEG_LEFT, LEG_RIGHT, FOOT_LEFT, FOOT_RIGHT)) withDefault { x => Seq(x) }

    val woundCanBe = Set(EYES, SKULL, FACE, NECK, LEG_LEFT, LEG_RIGHT, ARM_LEFT, ARM_RIGHT, CHEST, VITALS, ABDOMEN,
      GROIN, HAND_LEFT, HAND_RIGHT, FOOT_LEFT, FOOT_RIGHT)
    val canBe      = Set(EYES, SKULL, FACE, HEAD, NECK, LEG_LEFT, LEG_RIGHT, LEGS, ARM_LEFT, ARM_RIGHT, ARMS, CHEST,
      VITALS, ABDOMEN, GROIN, TORSO, HANDS, HAND_LEFT, HAND_RIGHT, FEET, FOOT_LEFT, FOOT_RIGHT, SKIN, BODY)
  }

}
