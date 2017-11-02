package models.charlist
package equipment

trait Possession {
  val state : String
  val broken: Boolean

  def totalCost: Double

  def totalWt: Double
}
