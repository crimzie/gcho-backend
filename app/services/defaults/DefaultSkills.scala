package services
package defaults

import models.charlist.bonuses.{DamageBonus, ReactionBonus}
import models.charlist.skill.Skill
import models.charlist.{Feature, FeatureEntry}
import play.api.libs.json.JsObject

import scala.language.postfixOps
import scala.xml.XML

object DefaultSkills {
  def parse(filePath: String): Stream[JsObject] =
    for {skl <- (XML load (getClass getResourceAsStream filePath)) \ "skill" toStream} yield {
      val (attr, diff) = (skl \ "difficulty" text) partition (_ != '/')
      val fs: FeatureEntry[Feature] = FeatureEntry(
        data = Skill(
          name = (skl \ "name").text,
          spc = (skl \ "specialization").headOption map (_.text),
          tl = (skl \ "tech_level").headOption map (_.text) withFilter (_.nonEmpty) map (_.asInt),
          attr = attr,
          diff = diff drop 1,
          dmgBonuses = for {b <- skl \ "weapon_bonus"} yield DamageBonus(
            skill = (b \ "name").text,
            skillCompare = (b \ "name" \ "@compare").text,
            spc = (b \ "specialization").headOption map (_.text),
            spcCompare = (b \ "specialization" \ "@compare").headOption map (_.text),
            minRelSkill = (b \ "level").headOption map (_.text) withFilter (_.nonEmpty) map (_.asInt),
            perDie = (b \ "amount" \ "@per_level").headOption map (_.text) withFilter (_ == "yes") map
              (_ => (b \ "amount").text.asInt),
            minBonus = (b \ "amount").text.asInt),
          reactBonuses = for (b <- skl \ "reaction_bonus") yield ReactionBonus(
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
      FeatureEntry.format writes fs
    } // TODO: missing prerequisites and defaults parsers
}
