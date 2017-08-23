package services
package defaults

import models.charlist.Charlist.flaggedWeaponFormat
import models.charlist.DamageType._
import models.charlist._
import play.api.libs.json.{JsObject, Json}

import scala.language.postfixOps
import scala.util.matching.Regex
import scala.xml.{Node, XML}

object DefaultWeapons {
  def parse(filePath: String): Stream[JsObject] = {
    val damageRgx: Regex =
      """[a-zA-Z]*(?>\+?(\d+)d)?(?>x(\d+))?([\+\-]\d+)?(?>\((\d+\.?\d*)\))?(?> ([a-z \+\-]*))?(?>\[(\d+)d\])?\-?""".r
    val parryRgx: Regex = """([\+\-]?\d)?(No|F|U)?""".r
    val stRgx: Regex = """(\d+)?(.*)""".r
    val accRgx: Regex = """(\d+)?(?>\+(\d))?\-?""".r
    val rofRgx: Regex = """(\d+)?(?>x(\d+))?(\!)?.*""".r
    val shotsRgx: Regex = """(\d+)?((?>\+1)?T?\([0-9a-z\-]+\))?""".r
    val dType: Map[String, String] =
      Map(
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
    val skl: Node => String = _ \ "default" collectFirst {
      case n: Node if !((n \ "name").text startsWith "!") && (n \ "modifier").text.asInt == 0 => (n \ "name").text
    } getOrElse ""
    val spc: Node => String = _ \ "default" collectFirst {
      case n: Node if !((n \ "name").text startsWith "!") && (n \ "modifier").text.asInt == 0 =>
        (n \ "specialization").text
    } getOrElse ""
    for {
      wpn <- (XML load (getClass getResourceAsStream filePath)) \ "equipment" toStream;
      if (wpn \ "melee_weapon").nonEmpty || (wpn \ "ranged_weapon").nonEmpty
    } yield Json toJsObject FlaggedWeapon(
      data = Weapon(
        name = (wpn \ "description").text,
        attacksMelee = for {a <- wpn \ "melee_weapon"} yield {
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
              dmgDice = dice.asInt,
              dmgMod = mod.asInt,
              armorDiv = div.asDbl,
              dmgType = dType(typ)),
            skill = skl(a),
            spc = spc(a),
            parry = par.asInt,
            parryType = parT,
            st = str.asInt,
            hands = hnd,
            reach = (a \ "reach").text)
        },
        attacksRanged = for {a <- wpn \ "ranged_weapon"} yield {
          val damageRgx(dice, mult, mod, div, typ, frag) = (a \ "damage").text
          val stRgx(str, hnd) = (a \ "strength").text
          val accRgx(ac, acm) = (a \ "accuracy").text
          val rofRgx(rf, mlt, fa) = (a \ "rate_of_fire").text
          val shotsRgx(sh, rld) = (a \ "shots").text
          RangedAttack(
            damage = RangedDamage(
              dmgDice = dice.asInt,
              diceMult = mult.asInt,
              dmgMod = mod.asInt,
              armorDiv = div.asDbl,
              fragDice = frag.asInt,
              dmgType = dType(typ)),
            skill = skl(a),
            spc = spc(a),
            st = str.asInt,
            hands = hnd,
            jet = (a \ "rate_of_fire").text == "Jet",
            acc = ac.asInt,
            accMod = acm.asInt,
            rng = (a \ "range").text,
            rof = RangedRoF(rf.asInt, mlt.asInt, fa == "!"),
            rcl = (a \ "recoil").text.asInt,
            shots = RangedShots(sh.asInt, rld, sh.asInt)) // TODO: WPS, CPS, malf
        },
        blocks = for (b <- wpn \ "melee_weapon" if (b \ "block").text == "+0") yield BlockDefence(
          name = (b \ "usage").text,
          skill = skl(b),
          spc = spc(b),
          db = (wpn \ "attribute_bonus" \ "amount").text.asInt),
        bulk = (wpn \ "ranged_weapon" \ "bulk").text.asInt,
        lc = (wpn \ "legality_class").text.asInt,
        tl = (wpn \ "tech_level").text.asInt,
        wt = (wpn \ "weight").text.replace(" lb", "").asDbl,
        cost = (wpn \ "value").text.asDbl),
      ready = true)
  }
}
