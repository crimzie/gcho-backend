import models.charlist._
import play.api.libs.json.Writes

import scala.xml.XML

/**
  * Created by crimson on 12/17/16.
  */
class TechniquesParser(filePath: String) extends Parser[FlaggedTechnique] {
  println("Parsing techniques...")
  override val seq: Seq[FlaggedTechnique] = for (tcn <- (XML load (getClass getResourceAsStream filePath)) \ "technique")
    yield FlaggedTechnique(
      data = Technique(
        name = (tcn \ "name").text,
        skill = (tcn \ "default" \ "name").text,
        spc = (tcn \ "default" \ "specialization").text,
        diff = (tcn \ "difficulty").text,
        defLvl = parseInt((tcn \ "default" \ "modifier").text), // TODO: defaults to stats not handled
        relLvl = parseInt((tcn \ "default" \ "modifier").text) + 1), // TODO: max level missing in lib
      ready = !(tcn toString() contains '@'))

  override val tjs: Writes[FlaggedTechnique] = Charlist.flaggedTechniqueFormat
}
