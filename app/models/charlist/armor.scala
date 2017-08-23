package models
package charlist

import scala.language.postfixOps
import scala.util.Random

case class Armor(
                  name: String = "",
                  var state: String = ItemState.EQUIPPED,
                  var db: Int = 0,
                  components: Seq[ArmorComponent] = Seq(ArmorComponent()),
                  var hp: Int = 1,
                  var hpLeft: Int = 1,
                  broken: Boolean = false,
                  var lc: Int = 5,
                  var tl: Int = 0,
                  notes: String = "",
                  var wt: Double = 0,
                  var cost: Double = 0) extends Possession {
  if (ItemState canBe state) () else state = ItemState.EQUIPPED
  if (db < 0) db = 0 else if (db > 3) db = 3
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0

  override def totalCost: Double = cost

  override def totalWt: Double = wt
}

case class FlaggedArmor(
                         _id: String = Random.alphanumeric take 8 mkString,
                         ready: Boolean,
                         override val data: Armor,
                         override var category: String = "",
                         override var name: String = "",
                         override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Armor, FlaggedArmor] {
  category = FlaggedFeature.ARMOR
  name = data.name

  override def updated(user: String): FlaggedArmor = copy(user = user)
}

case class ArmorComponent(
                           protection: DrSet = DrSet(),
                           var front: Boolean = true,
                           back: Boolean = true,
                           var drType: String = DrType.HARD,
                           var locations: Seq[String] = Nil) {
  if (!front && !back) front = true
  if (DrType canBe drType) () else drType = DrType.HARD
  locations = locations.distinct filter HitLocation.canBe
}

object DrType {
  val HARD = "hard"
  val SOFT = "soft"
  val FIELD = "force field"
  val SKIN = "tough skin"
  val canBe = Set(HARD, SOFT, FIELD, SKIN)
}

object HitLocation {
  val EYES = "eyes"
  val SKULL = "skull"
  val FACE = "face"
  val HEAD = "head"
  val NECK = "neck"
  val LEG_RIGHT = "right leg"
  val LEG_LEFT = "left leg"
  val LEGS = "legs"
  val ARM_RIGHT = "right arm"
  val ARM_LEFT = "left arm"
  val ARMS = "arms"
  val CHEST = "chest"
  val VITALS = "vitals"
  val ABDOMEN = "abdomen"
  val GROIN = "groin"
  val TORSO = "torso"
  val HANDS = "hands"
  val HAND_LEFT = "left hand"
  val HAND_RIGHT = "right hand"
  val FEET = "feet"
  val FOOT_RIGHT = "right foot"
  val FOOT_LEFT = "left foot"
  val SKIN = "skin"
  val BODY = "full body"
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
  val woundCanBe = Set(EYES, SKULL, FACE, NECK, LEG_LEFT, LEG_RIGHT, ARM_LEFT, ARM_RIGHT, CHEST, VITALS, ABDOMEN, GROIN,
    HAND_LEFT, HAND_RIGHT, FOOT_LEFT, FOOT_RIGHT)
  val canBe = Set(EYES, SKULL, FACE, HEAD, NECK, LEG_LEFT, LEG_RIGHT, LEGS,
    ARM_LEFT, ARM_RIGHT, ARMS, CHEST, VITALS, ABDOMEN, GROIN, TORSO, HANDS, HAND_LEFT, HAND_RIGHT, FEET, FOOT_LEFT,
    FOOT_RIGHT, SKIN, BODY)
}
