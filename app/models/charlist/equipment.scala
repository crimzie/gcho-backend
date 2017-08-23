package models
package charlist

case class Equipment(
                      weapons: Seq[Weapon] = Nil,
                      armor: Seq[Armor] = Nil,
                      items: Seq[Item] = Nil,
                      var totalDb: Int = 0,
                      var totalCost: Double = 0,
                      var totalCombWt: Double = 0,
                      var totalTravWt: Double = 0,
                      combat: Boolean = false) {

  import ItemState._

  private val weights = (f: String => Boolean) =>
    weapons.filter(!_.innate) ++ armor ++ items collect { case p if f(p.state) => p.totalWt }
  totalCost = (0.0 /: (weapons.filter(!_.innate) ++ armor ++ items)) (_ + _.totalCost)
  totalCombWt = weights(Set(READY, EQUIPPED, COMBAT)).sum
  totalTravWt = totalCombWt + weights(_ == TRAVEL).sum
  private val equip = (p: Possession) => Set(READY, EQUIPPED)(p.state) && !p.broken
  totalDb = (weapons.withFilter(equip).flatMap(_.blocks).map(_.db) ++ armor.withFilter(equip).map(_.db)).sum
}

/** Parent trait for all "equipment item" classes that simplifies total equipment cost and weight aggregation. */
trait Possession {
  var state: String
  val broken: Boolean

  def totalCost: Double

  def totalWt: Double
}

object ItemState {
  val READY = "Ready"
  val EQUIPPED = "Equipped"
  val COMBAT = "Combat"
  val TRAVEL = "Travel"
  val STASH = "Stash"
  val canBe = Set(READY, EQUIPPED, COMBAT, TRAVEL, STASH)
}
