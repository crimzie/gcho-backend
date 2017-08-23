package services
package defaults

import models.charlist._
import play.api.libs.json.{JsObject, Json, Writes}

import scala.language.postfixOps
import scala.xml.XML
import Charlist.flaggedSkillFormat

object DefaultSkills {
  def parse(filePath: String): Stream[JsObject] =
    for {skl <- (XML load (getClass getResourceAsStream filePath)) \ "skill" toStream} yield {
      val (attr, diff) = (skl \ "difficulty" text) partition (_ != '/')
      val fs = FlaggedSkill(
        data = Skill(
          name = (skl \ "name").text,
          spc = (skl \ "specialization").text,
          tl = (skl \ "tech_level").size,
          attr = attr,
          diff = diff drop 1,
          dmgBonuses = for {b <- skl \ "weapon_bonus"} yield BonusDamage(
            skill = (b \ "name").text,
            skillCompare = (b \ "name" \ "@compare").text,
            spc = (b \ "specialization").text,
            spcCompare = (b \ "specialization" \ "@compare").text,
            relSkill = (b \ "level").text.asInt,
            perDie = (b \ "amount" \ "@per_level").text == "yes",
            bonus = (b \ "amount").text.asInt),
          reactBonuses = for (b <- skl \ "reaction_bonus") yield BonusReaction(
            affected = (b \ "affected").text,
            reputation = (b \ "affected" \ "@reputation").text == "yes",
            perLvl = (b \ "amount" \ "@per_level").text == "yes",
            freq = (b \ "frequency").text.asInt,
            bonus = (b \ "amount").text.asInt,
            notes = (b \ "notes").text),
          encumbr = (skl \ "encumbrance_penalty_multiplier").text.asInt > 0,
          categories = (skl \ "categories" \ "category") map (_.text),
          notes = (skl \ "notes").text),
        ready = !(skl toString() contains '@') && (skl \ "tech_level").isEmpty)
      Json toJsObject fs
    } // TODO: missing prerequisites and defaults parsers
}
