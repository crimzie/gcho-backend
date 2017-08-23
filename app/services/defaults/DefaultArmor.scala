package services
package defaults

import models.charlist.Charlist.flaggedArmorFormat
import models.charlist.{Armor, ArmorComponent, DrSet, FlaggedArmor}
import play.api.libs.json.{JsObject, Json}

import scala.language.postfixOps
import scala.xml.XML

object DefaultArmor {
  def parse(filePath: String): Stream[JsObject] = for {
    arm <- (XML load (getClass getResourceAsStream filePath)) \ "equipment" toStream;
    if arm \ "dr_bonus" nonEmpty
  } yield Json toJsObject FlaggedArmor(
    data = Armor(
      name = s"${arm \ "description" text}/TL${arm \ "tech_level" text}",
      components = for {c <- arm \ "dr_bonus"} yield ArmorComponent(
        protection = DrSet((c \ "amount").text.asInt, (c \ "ep").text.asInt, (c \ "epi").text.asInt),
        locations = c \ "location" map (_.text)),
      lc = (arm \ "legality_class").text.asInt,
      tl = (arm \ "tech_level").text.asInt,
      wt = (arm \ "weight").text.replace(" lb", "").asDbl,
      cost = (arm \ "value").text.asDbl),
    ready = true) // TODO: fields missing (rigdity, ep, epi, front/back, category)
}
