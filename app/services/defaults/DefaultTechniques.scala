package services
package defaults

import models.charlist.Charlist.flaggedTechniqueFormat
import models.charlist.{FlaggedTechnique, Technique}
import play.api.libs.json.{JsObject, Json}

import scala.language.postfixOps
import scala.xml.XML

object DefaultTechniques {
  def parse(filePath: String): Stream[JsObject] =
    for {tcn <- (XML load (getClass getResourceAsStream filePath)) \ "technique" toStream}
      yield Json toJsObject FlaggedTechnique(
        data = Technique(
          name = (tcn \ "name").text,
          skill = (tcn \ "default" \ "name").text,
          spc = (tcn \ "default" \ "specialization").text,
          diff = (tcn \ "difficulty").text,
          defLvl = (tcn \ "default" \ "modifier").text.asInt, // TODO: defaults to stats not handled
          relLvl = (tcn \ "default" \ "modifier").text.asInt + 1), // TODO: max level missing in lib
        ready = !(tcn toString() contains '@'))
}
