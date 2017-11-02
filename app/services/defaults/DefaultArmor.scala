package services
package defaults

import models.charlist.FeatureEntry
import models.charlist.armor.{Armor, ArmorComponent}
import models.charlist.dr.DrSet
import play.api.libs.json.JsObject

import scala.language.postfixOps
import scala.xml.XML

object DefaultArmor {
  def parse(filePath: String): Stream[JsObject] = for {
    arm <- XML.load(getClass getResourceAsStream filePath) \ "equipment" toStream;
    if arm \ "dr_bonus" nonEmpty
  } yield FeatureEntry.format writes FeatureEntry(
    data = Armor(
      name = s"${arm \ "description" text}/TL${arm \ "tech_level" text}",
      components = for {c <- arm \ "dr_bonus"} yield ArmorComponent(
        protection = DrSet((c \ "amount").text.asInt, (c \ "ep").text.asInt, (c \ "epi").text.asInt),
        locations = c \ "location" map (_.text) toSet),
      lc = (arm \ "legality_class").text.asInt,
      tl = (arm \ "tech_level").text.asInt,
      wt = (arm \ "weight").text.replace(" lb", "").asDbl,
      cost = (arm \ "value").text.asDbl),
    ready = true) // TODO: fields missing (rigdity, ep, epi, front/back, category)
}
