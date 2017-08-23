package models
package charlist

case class DamageResistance(
                             skull: HitLocationDR = HitLocationDR(),
                             eyes: HitLocationDR = HitLocationDR(),
                             face: HitLocationDR = HitLocationDR(),
                             neck: HitLocationDR = HitLocationDR(),
                             armLeft: HitLocationDR = HitLocationDR(),
                             armRight: HitLocationDR = HitLocationDR(),
                             handLeft: HitLocationDR = HitLocationDR(),
                             handRight: HitLocationDR = HitLocationDR(),
                             chest: HitLocationDR = HitLocationDR(),
                             vitals: HitLocationDR = HitLocationDR(),
                             abdomen: HitLocationDR = HitLocationDR(),
                             groin: HitLocationDR = HitLocationDR(),
                             legLeft: HitLocationDR = HitLocationDR(),
                             legRight: HitLocationDR = HitLocationDR(),
                             footLeft: HitLocationDR = HitLocationDR(),
                             footRight: HitLocationDR = HitLocationDR())

case class HitLocationDR(front: DrSet = DrSet(), rear: DrSet = DrSet())

case class DrSet(var dr: Int = 0, var ep: Int = 0, var epi: Int = 0) {
  if (dr < 0) dr = 0
  if (ep < 0) ep = 0
  if (epi < 0) epi = 0

  def +(that: DrSet): DrSet = DrSet(this.dr + that.dr, this.ep + that.ep, this.epi + that.epi)

  def *(x: Int): DrSet = DrSet(this.dr * x, this.ep * x, this.epi * x)
}
