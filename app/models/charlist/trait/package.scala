package models.charlist

package object `trait` {

  object TraitSwitch {
    val ALWAYSON   = "Always on"
    val SWITCHABLE = "Switchable"
    val CONTROL    = "Roll"
    val ATTACK     = "Attack"

    val canBe = Set(ALWAYSON, SWITCHABLE, CONTROL, ATTACK)
  }

  object TraitType {
    val MENTAL   = "Mental"
    val PHYSICAL = "Physical"
    val SOCIAL   = "Social"
    val MUNDANE  = "Mundane"
    val EXOTIC   = "Exotic"
    val SUPER    = "Supernatural"

    val canBe = Set(MENTAL, PHYSICAL, SOCIAL, MUNDANE, EXOTIC, SUPER)
  }

  object TraitCategory {
    val RACE         = "Race"
    val ADVANTAGE    = "Advantage"
    val DISADVANTAGE = "Disadvantage"
    val PERK         = "Perk"
    val QUIRK        = "Quirk"
    val LANGUAGE     = "Language"

    val canBe = Set(RACE, ADVANTAGE, DISADVANTAGE, PERK, QUIRK, LANGUAGE)
  }

  object TraitModifierCategory {
    val DEFAULT  = "base"
    val MODIFIER = "modifier"
    val VARIANT  = "variant"

    val canBe = Set(DEFAULT, MODIFIER, VARIANT)
  }

  object TraitModifierAffects {
    val TOTAL  = "total"
    val BASE   = "base"
    val LEVELS = "levels only"

    val canBe = Set(TOTAL, BASE, LEVELS)
  }

  object TraitModifierCostType {
    val PERCENT    = "percentage"
    val LEVEL      = "percentage per level"
    val POINTS     = "points"
    val MULTIPLIER = "multiplier"

    val canBe = Set(PERCENT, LEVEL, POINTS, MULTIPLIER)
  }

}
