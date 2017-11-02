package services
package defaults

import models.charlist.{Feature, FeatureEntry}
import models.charlist.`trait`._
import models.charlist.bonuses._
import models.charlist.dr.DrSet
import play.api.libs.json.{JsObject, Json}

import scala.collection.immutable.Seq
import scala.language.postfixOps
import scala.xml.{Node, XML}

object DefaultTraits {
  private def parseTraitMod(n: Node): (
    Seq[AttributeBonus],
      Seq[SkillBonus],
      Seq[DamageBonus],
      Seq[DRBonus],
      Seq[AttributeCostBonus],
      Seq[ReactionBonus]) = ( // TODO: weapon parser missing
    for {b <- n \ "attribute_bonus"} yield AttributeBonus(
      attr = (b \ "attribute").text,
      bonus = (b \ "amount").text.asDbl,
      perLvl = (b \ "amount" \ "@per_level").text == "yes"),
    for {b <- n \ "skill_bonus"} yield SkillBonus(
      skill = (b \ "name").text,
      skillCompare = (b \ "name" \ "@compare").text,
      spc = (b \ "specialization").headOption map (_.text),
      spcCompare = (b \ "specialization" \ "@compare").headOption map (_.text),
      perLvl = (b \ "amount" \ "@per_level").text == "yes",
      bonus = (b \ "amount").text.asInt),
    for {b <- n \ "weapon_bonus"} yield DamageBonus(
      skill = (b \ "name").text,
      skillCompare = (b \ "name" \ "@compare").text,
      spc = (b \ "specialization").headOption map (_.text),
      spcCompare = (b \ "specialization" \ "@compare").headOption map (_.text),
      minRelSkill = (b \ "level").headOption map (_.text.toInt),
      perDie = (b \ "amount" \ "@per_die").headOption flatMap { n =>
        if (n.text == "yes") (b \ "amount").headOption map (_.text) withFilter (_.nonEmpty) map (_.asInt) else None
      },
      minBonus = (b \ "amount").text.asInt),
    for (b <- n \ "dr_bonus") yield DRBonus(
      locations = Seq((b \ "location").text),
      perLvl = (b \ "amount" \ "@per_level").text == "yes",
      protection = DrSet((b \ "amount").text.asInt)),
    for (b <- n \ "cost_reduction") yield AttributeCostBonus(
      attr = (b \ "attribute").text,
      cost = (b \ "percentage").text.asInt),
    for (b <- n \ "reaction_bonus") yield ReactionBonus(
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
      variants = Some("CR"),
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
          name = "Base",
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
          variants = (mod \ "variant").headOption map (_.text),
          name = (mod \ "name").text,
          ref = (mod \ "reference").text,
          notes = (mod \ "notes").text,
          level = math.max((mod \ "levels").text.asInt, 1),
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
      val ft: FeatureEntry[Feature] = FeatureEntry(
        data = Trait(
          name = (adv \ "name").text,
          types = (adv \ "type").text split ", " flatMap (_ split "/"),
          category = adv \ "categories" \ "category" map (_.text) match {
            case x if x contains "Advantage"    => "Advantage"
            case x if x contains "Disadvantage" => "Disadvantage"
            case x                              => x.headOption getOrElse ""
          }, // TODO: duplicate ambiguous traits
          switch = (adv \ "name" \ "@switchability").text,
          ref = (adv \ "reference").text,
          notes = (adv \ "notes").text,
          active = false,
          cpBase = (adv \ "base_points").text.asInt,
          level = (adv \ "levels").headOption map (_.text.asInt),
          cpPerLvl = (adv \ "points_per_level").headOption map (_.text) withFilter (_.nonEmpty) map (_.asInt),
          modifiers = defMod ++ ((adv \ "cr") flatMap { _ => variants }) ++ modifiers),
        ready = (adv \ "cr").isEmpty && (adv \ "modifier").isEmpty && !(adv.toString contains '@'))
      FeatureEntry.format writes ft
    }
  }
}
