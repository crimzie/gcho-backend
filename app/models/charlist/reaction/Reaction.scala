package models.charlist
package reaction

import play.api.libs.json.{Format, Json}

case class Reaction(affected: String = "Everyone", modifiers: Seq[ReactionMod] = Nil)

object Reaction {
  implicit val format: Format[Reaction] = Json.format[Reaction]
}
