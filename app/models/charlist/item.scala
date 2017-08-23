package models
package charlist

import scala.language.postfixOps
import scala.util.Random

case class Item(
                 name: String = "",
                 var state: String = ItemState.STASH,
                 var dr: Int = 0,
                 var hp: Int = 1,
                 var hpLeft: Int = 1,
                 broken: Boolean = false,
                 var lc: Int = 5,
                 var tl: Int = 0,
                 notes: String = "",
                 var wt: Double = 0,
                 var cost: Double = 0,
                 var n: Int = 1,
                 var totalWt: Double = 0,
                 var totalCost: Double = 0)
  extends Possession {
  if (ItemState canBe state) () else state = ItemState.STASH
  if (dr < 0) dr = 0
  if (hp < 0) hp = 0
  if (hpLeft < 0) hpLeft = 0 else if (hpLeft > hp) hpLeft = hp
  if (lc > 5) lc = 5 else if (lc < 0) lc = 0
  if (tl < 0) tl = 0 else if (tl > 12) tl = 12
  if (wt < 0) wt = 0
  if (cost < 0) cost = 0
  if (n < 0) n = 0
  totalWt = wt * n
  totalCost = cost * n
}

case class FlaggedItem(
                        _id: String = Random.alphanumeric take 8 mkString,
                        ready: Boolean,
                        data: Item,
                        override var category: String = "",
                        override var name: String = "",
                        override val user: String = FlaggedFeature.DEF_USER_VAL)
  extends FlaggedFeature[Item, FlaggedItem] {
  category = FlaggedFeature.ITEMS
  name = data.name

  override def updated(user: String): FlaggedItem = copy(user = user)
}

