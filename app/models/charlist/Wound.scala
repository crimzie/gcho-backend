package models
package charlist

case class Wound(
                  var location: String = HitLocation.CHEST,
                  var dType: String = DamageType.CRUSHING,
                  var points: Int = 1,
                  firstAid: Boolean = false,
                  bleeding: Boolean = false,
                  crippling: Boolean = false,
                  lasting: Boolean = false) {
  if (HitLocation woundCanBe location) () else location = HitLocation.CHEST
  if (DamageType canBe dType) () else dType = DamageType.CRUSHING
  if (points < 1) points = 1
}
