import models.charlist.DamageType._
import models.charlist._
import play.api.libs.json.Writes

import scala.xml.XML

/**
  * Created by crimson on 12/14/16.
  */
class WeaponsParser(filePath: String) extends Parser[FlaggedWeapon] {
  println("Parsing weapons...")
  private val damageRgx =
    """[a-zA-Z]*(?>\+?(\d+)d)?(?>x(\d+))?([\+\-]\d+)?(?>\((\d+\.?\d*)\))?(?> ([a-z \+\-]*))?(?>\[(\d+)d\])?\-?""".r
  private val parryRgx = """([\+\-]?\d)?(No|F|U)?""".r
  private val stRgx = """(\d+)?(.*)""".r
  private val accRgx = """(\d+)?(?>\+(\d))?\-?""".r
  private val rofRgx = """(\d+)?(?>x(\d+))?(\!)?.*""".r
  private val shotsRgx = """(\d+)?((?>\+1)?T?\([0-9a-z\-]+\))?""".r
  private val dType = Map(
    "cr ex" -> CRUSHING_EXPLOSION,
    "burn ex" -> BURNING_EXPLOSION,
    "pi++" -> PIERCING_HUGE,
    "pi+" -> PIERCING_LARGE,
    "pi" -> PIERCING,
    "pi-" -> PIERCING_SMALL,
    "cr" -> CRUSHING,
    "cut" -> CUTTING,
    "imp" -> IMPALING,
    "burn" -> BURNING,
    "tox" -> TOXIC,
    "cor" -> CORROSION,
    "aff" -> AFFLICTION,
    "fat" -> FATIGUE) withDefaultValue SPECIAL
  override val seq: Seq[FlaggedWeapon] =
    for (wpn <- (XML load (getClass getResourceAsStream filePath)) \ "equipment"
         if (wpn \ "melee_weapon").nonEmpty || (wpn \ "ranged_weapon").nonEmpty)
      yield FlaggedWeapon(
        data = Weapon(
          name = (wpn \ "description").text,
          attacksMelee = for (a <- wpn \ "melee_weapon") yield {
            val damageRgx(dice, _, mod, div, typ, _) = (a \ "damage").text
            val parryRgx(par, parT) = (a \ "parry").text
            val stRgx(str, hnd) = (a \ "strength").text
            MeleeAttack(
              name = (a \ "usage").text,
              damage = MeleeDamage(// TODO: grips
                attackType = (a \ "damage").text match {
                  case x if x startsWith "thr" => AttackType.THRUSTING
                  case x if x startsWith "sw" => AttackType.SWINGING
                  case _ => AttackType.WEAPON
                },
                dmgDice = parseInt(dice),
                dmgMod = parseInt(mod),
                armorDiv = parseDouble(div),
                dmgType = dType(typ)),
              skill = (for (d <- a \ "default" if !((d \ "name").text contains '!'))
                yield parseInt((d \ "modifier").text) -> (d \ "name").text).toMap withDefaultValue "" apply 0,
              spc = (for (d <- a \ "default" if !((d \ "name").text contains '!'))
                yield parseInt((d \ "modifier").text) -> (d \ "specialization").text).toMap withDefaultValue "" apply 0,
              parry = parseInt(par),
              parryType = parT,
              st = parseInt(str),
              hands = hnd,
              reach = (a \ "reach").text)
          },
          attacksRanged = for (a <- wpn \ "ranged_weapon") yield {
            val damageRgx(dice, mult, mod, div, typ, frag) = (a \ "damage").text
            val stRgx(str, hnd) = (a \ "strength").text
            val accRgx(ac, acm) = (a \ "accuracy").text
            val rofRgx(rf, mlt, fa) = (a \ "rate_of_fire").text
            val shotsRgx(sh, rld) = (a \ "shots").text
            RangedAttack(
              damage = RangedDamage(
                dmgDice = parseInt(dice),
                diceMult = parseInt(mult),
                dmgMod = parseInt(mod),
                armorDiv = parseDouble(div),
                fragDice = parseInt(frag),
                dmgType = dType(typ)),
              skill = (for (d <- a \ "default" if !((d \ "name").text contains '!'))
                yield parseInt((d \ "modifier").text) -> (d \ "name").text).toMap withDefaultValue "" apply 0,
              spc = (for (d <- a \ "default" if !((d \ "name").text contains '!'))
                yield parseInt((d \ "modifier").text) -> (d \ "specialization").text).toMap withDefaultValue "" apply 0,
              st = parseInt(str),
              hands = hnd,
              jet = (a \ "rate_of_fire").text == "Jet",
              acc = parseInt(ac),
              accMod = parseInt(acm),
              rng = (a \ "range").text,
              rof = RangedRoF(parseInt(rf), parseInt(mlt), fa == "!"),
              rcl = parseInt((a \ "recoil").text),
              shots = RangedShots(parseInt(sh), rld, parseInt(sh))) // TODO: WPS, CPS, malf
          },
          blocks = for (b <- wpn \ "melee_weapon" if (b \ "block").text == "+0") yield BlockDefence(
            name = (b \ "usage").text,
            skill = (for (d <- b \ "default" if !((d \ "name").text contains '!'))
              yield parseInt((d \ "modifier").text) -> (d \ "name").text).toMap withDefaultValue "" apply 0,
            spc = (for (d <- b \ "default" if !((d \ "name").text contains '!'))
              yield parseInt((d \ "modifier").text) -> (d \ "specialization").text).toMap withDefaultValue "" apply 0,
            db = parseInt((wpn \ "attribute_bonus" \ "amount").text)),
          bulk = parseInt((wpn \ "ranged_weapon" \ "bulk").text),
          lc = parseInt((wpn \ "legality_class").text),
          tl = parseInt((wpn \ "tech_level").text),
          wt = parseDouble((wpn \ "weight").text.replace(" lb", "")),
          cost = parseDouble((wpn \ "value").text)),
        ready = true)

  override val tjs: Writes[FlaggedWeapon] = Charlist.flaggedWeaponFormat
}
