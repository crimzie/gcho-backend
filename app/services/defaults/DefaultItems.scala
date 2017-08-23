package services
package defaults

import models.charlist.Charlist.flaggedItemFormat
import models.charlist.{FlaggedItem, Item}
import play.api.libs.json.{JsObject, Json}

import scala.language.postfixOps
import scala.xml.XML

object DefaultItems {
  def parse(filePath: String): Stream[JsObject] = for {
    itm <- (XML load (getClass getResourceAsStream filePath)) \ "equipment" toStream;
    if (itm \ "melee_weapon").isEmpty && (itm \ "ranged_weapon").isEmpty && (itm \ "dr_bonus").isEmpty
  } yield Json toJsObject FlaggedItem(
    data = Item(
      name = itm \ "description" text,
      lc = (itm \ "legality_class").text.asInt,
      tl = (itm \ "tech_level").text.asInt,
      wt = (itm \ "weight").text.replace(" lb", "").asDbl,
      cost = (itm \ "value").text.asDbl),
    ready = true)
}
