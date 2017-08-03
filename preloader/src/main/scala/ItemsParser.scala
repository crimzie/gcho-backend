import models.charlist._
import play.api.libs.json.Writes

import scala.xml.XML

/**
  * Created by crimson on 12/17/16.
  */
class ItemsParser(filePath: String) extends Parser[FlaggedItem] {
  println("Parsing items...")
  override val seq: Seq[FlaggedItem] =
    for (itm <- (XML load (getClass getResourceAsStream filePath)) \ "equipment"
         if (itm \ "melee_weapon").isEmpty && (itm \ "ranged_weapon").isEmpty && (itm \ "dr_bonus").isEmpty)
      yield FlaggedItem(
        data = Item(
          name = (itm \ "description").text,
          lc = parseInt((itm \ "legality_class").text),
          tl = parseInt((itm \ "tech_level").text),
          wt = parseDouble((itm \ "weight").text replace(" lb", "")),
          cost = parseDouble((itm \ "value").text)),
        ready = true)

  override val tjs: Writes[FlaggedItem] = Charlist.flaggedItemFormat
}
