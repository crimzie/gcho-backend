package models
package charlist

case class Reaction(affected: String = "", modifiers: Seq[ReactionMod] = Nil)

case class ReactionMod(freq: Int = 16, mod: Int = 0, notes: String = "")
