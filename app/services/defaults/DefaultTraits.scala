package services
package defaults

import models.charlist.Charlist.flaggedTraitFormat
import models.charlist._
import play.api.libs.json.{JsObject, Json}

import scala.collection.immutable.Seq
import scala.language.postfixOps
import scala.xml.{Node, XML}

object DefaultTraits {
  private def parseTraitMod(n: Node): (
    Seq[BonusAttribute],
      Seq[BonusSkill],
      Seq[BonusDamage],
      Seq[BonusDR],
      Seq[BonusAttributeCost],
      Seq[BonusReaction]) = ( // TODO: weapon parser missing
    for {b <- n \ "attribute_bonus"} yield BonusAttribute(
      attr = (b \ "attribute").text,
      bonus = (b \ "amount").text.asDbl,
      perLvl = (b \ "amount" \ "@per_level").text == "yes"),
    for {b <- n \ "skill_bonus"} yield BonusSkill(
      skill = (b \ "name").text,
      skillCompare = (b \ "name" \ "@compare").text,
      spc = (b \ "specialization").text,
      spcCompare = (b \ "specialization" \ "@compare").text,
      perLvl = (b \ "amount" \ "@per_level").text == "yes",
      bonus = (b \ "amount").text.asInt),
    for {b <- n \ "weapon_bonus"} yield BonusDamage(
      skill = (b \ "name").text,
      skillCompare = (b \ "name" \ "@compare").text,
      spc = (b \ "specialization").text,
      spcCompare = (b \ "specialization" \ "@compare").text,
      relSkill = (b \ "level").text.asInt,
      perDie = (b \ "amount" \ "@per_die").text == "yes",
      bonus = (b \ "amount").text.asInt),
    for (b <- n \ "dr_bonus") yield BonusDR(
      locations = Seq((b \ "location").text),
      perLvl = (b \ "amount" \ "@per_level").text == "yes",
      protection = DrSet((b \ "amount").text.asInt)),
    for (b <- n \ "cost_reduction") yield BonusAttributeCost(
      attr = (b \ "attribute").text,
      cost = (b \ "percentage").text.asInt),
    for (b <- n \ "reaction_bonus") yield BonusReaction(
      affected = (b \ "affected").text,
      reputation = (b \ "affected" \ "@reputation").text == "yes",
      perLvl = (b \ "amount" \ "@per_level").text == "yes",
      freq = (b \ "frequency").text.asInt,
      bonus = (b \ "amount").text.asInt,
      notes = (b \ "notes").text))

  def parse(filePath: String): Stream[JsObject] = {
    val modifier = TraitModifier(
      on = false,
      cat = TraitModifierCategory.VARIANT,
      variants = "CR",
      name = "CR 6",
      ref = "BS121",
      costType = TraitModifierCostType.MULTIPLIER,
      cost = 2.0)
    val variants: Seq[TraitModifier] = Seq(
      modifier,
      modifier.copy(name = "CR 9", cost = 1.5),
      modifier.copy(on = true, name = "CR 12", cost = 1.0),
      modifier.copy(name = "CR 15", cost = 0.5))
    for {adv <- (XML load (getClass getResourceAsStream filePath)) \ "advantage" toStream} yield {
      val defMod: Seq[TraitModifier] = {
        val (atrBns, sklBns, dmgBns, drBns, atrCMd, rctBns) = parseTraitMod(adv)
        if ((atrBns :: sklBns :: dmgBns :: drBns :: atrCMd :: rctBns :: Nil) forall (_.isEmpty)) Nil
        else TraitModifier(
          cat = TraitModifierCategory.DEFAULT,
          attrBonuses = atrBns,
          skillBonuses = sklBns,
          dmgBonuses = dmgBns,
          drBonuses = drBns,
          attrCostMods = atrCMd,
          reactBonuses = rctBns) :: Nil
      }
      val modifiers: Seq[TraitModifier] = for {mod <- adv \ "modifier"} yield {
        val (atrBns, sklBns, dmgBns, drBns, atrCMd, rctBns) = parseTraitMod(mod)
        TraitModifier(
          on = false,
          cat = if ((mod \ "variant").isEmpty) "" else TraitModifierCategory.VARIANT,
          variants = (mod \ "variant").text,
          name = (mod \ "name").text,
          ref = (mod \ "reference").text,
          notes = (mod \ "notes").text,
          level = (mod \ "levels").text.asInt,
          attrBonuses = atrBns,
          skillBonuses = sklBns,
          dmgBonuses = dmgBns,
          drBonuses = drBns,
          attrCostMods = atrCMd,
          affects = (mod \ "affects").text,
          costType = (mod \ "cost" \ "@type").text,
          cost = (mod \ "cost").text.asDbl,
          reactBonuses = rctBns)
      }
      val ft = FlaggedTrait(
        data = Trait(
          name = (adv \ "name").text,
          types = (adv \ "type").text split ", " flatMap (_ split "/"),
          category = adv \ "categories" \ "category" map (_.text) match {
            case x if x contains "Advantage" => "Advantage"
            case x if x contains "Disadvantage" => "Disadvantage"
            case x => x.headOption getOrElse ""
          }, // TODO: duplicate ambiguous traits
          switch = (adv \ "name" \ "@switchability").text,
          ref = (adv \ "reference").text,
          notes = (adv \ "notes").text,
          active = false,
          cpBase = (adv \ "base_points").text.asInt,
          level = (adv \ "levels").text.asInt,
          cpPerLvl = (adv \ "points_per_level").text.asInt,
          modifiers = defMod ++ ((adv \ "cr") flatMap { _ => variants }) ++ modifiers),
        ready = (adv \ "cr").isEmpty && (adv \ "modifier").isEmpty && !(adv.toString contains '@'))
      Json toJsObject ft
    }
  }
}
