import models.charlist._
import play.api.libs.json.Writes

import scala.xml.XML

/**
  * Created by crimson on 12/8/16.
  */
class SkillsParser(filePath: String) extends Parser[FlaggedSkill] {
  println("Parsing skills...")
  val seq: Seq[FlaggedSkill] = for (skl <- (XML load (getClass getResourceAsStream filePath)) \ "skill") yield
    FlaggedSkill(
      data = Skill(
        name = (skl \ "name").text,
        spc = (skl \ "specialization").text,
        tl = (skl \ "tech_level").size,
        attr = (skl \ "difficulty").text takeWhile (_ != '/'),
        diff = (skl \ "difficulty").text dropWhile (_ != '/') drop 1,
        dmgBonuses = for (b <- skl \ "weapon_bonus") yield BonusDamage(
          skill = (b \ "name").text,
          skillCompare = (b \ "name" \ "@compare").text,
          spc = (b \ "specialization").text,
          spcCompare = (b \ "specialization" \ "@compare").text,
          relSkill = this parseInt (b \ "level").text,
          perDie = (b \ "amount" \ "@per_level").text == "yes",
          bonus = this parseInt (b \ "amount").text),
        reactBonuses = for (b <- skl \ "reaction_bonus") yield BonusReaction(
          affected = (b \ "affected").text,
          reputation = (b \ "affected" \ "@reputation").text == "yes",
          perLvl = (b \ "amount" \ "@per_level").text == "yes",
          freq = this parseInt (b \ "frequency").text,
          bonus = this parseInt (b \ "amount").text,
          notes = (b \ "notes").text),
        encumbr = (this parseInt (skl \ "encumbrance_penalty_multiplier").text) > 0,
        categories = (skl \ "categories" \ "category") map (_.text),
        notes = (skl \ "notes").text),
      ready = !(skl toString() contains '@') && (skl \ "tech_level").isEmpty) // TODO: missing prerequisites and defaults parsers

  override val tjs: Writes[FlaggedSkill] = Charlist.flaggedSkillFormat
}
