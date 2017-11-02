package models.charlist

import com.github.dwickern.macros.NameOf
import io.swagger.annotations.ApiModelProperty
import models.charlist.`trait`.Trait
import models.charlist.armor.Armor
import models.charlist.equipment.Item
import models.charlist.skill.{Skill, Technique}
import models.charlist.weapon.Weapon
import play.api.libs.functional.syntax._
import play.api.libs.json._

import scala.language.postfixOps
import scala.util.Random

trait Feature {
  val str: String
}

object Feature {
  private  val writes: Writes[Feature] = {
    case t: Technique => Technique.format.writes(t)
    case s: Skill     => Skill.format.writes(s)
    case a: Armor     => Armor.format.writes(a)
    case i: Item      => Item.format.writes(i)
    case t: Trait     => Trait.format.writes(t)
    case w: Weapon    => Weapon.format.writes(w)
  }
  private  val reads : Reads[Feature]  = jv => Technique.format.reads(jv) | Skill.format.reads(jv) |
    Armor.format.reads(jv) | Item.format.reads(jv) | Trait.format.reads(jv) | Weapon.format.reads(jv)
  implicit val format: Format[Feature] = Format(reads, writes)
}

case class FeatureEntry[T <: Feature](
    @ApiModelProperty(hidden = true)
    user: String = "",
    @ApiModelProperty(required = false)
    _id: String = FeatureEntry.randId(),
    data: T,
    ready: Boolean) {
  @ApiModelProperty(hidden = true)
  val cat: String = data match {
    case _: Technique => FeatureEntry.TECHNIQUES
    case _: Skill     => FeatureEntry.SKILLS
    case _: Armor     => FeatureEntry.ARMOR
    case _: Item      => FeatureEntry.ITEMS
    case _: Trait     => FeatureEntry.TRAITS
    case _: Weapon    => FeatureEntry.WEAPONS
  }
}

object FeatureEntry {
  private def randId() = Random.alphanumeric take 8 mkString

  val TRAITS    : String = "traits"
  val SKILLS    : String = "skills"
  val TECHNIQUES: String = "tecns"
  val ARMOR     : String = "armors"
  val WEAPONS   : String = "weaps"
  val ITEMS     : String = "items"

  private  val dataPath : JsPath                         = JsPath \ NameOf.nameOf[FeatureEntry[_]](_.data)
  private  val idPath   : JsPath                         = JsPath \ NameOf.nameOf[FeatureEntry[_]](_._id)
  private  val readyPath: JsPath                         = JsPath \ NameOf.nameOf[FeatureEntry[_]](_.ready)
  private  val reads    : Reads[FeatureEntry[Feature]]   = (
    Reads.pure("") ~
      idPath.readWithDefault(Random.alphanumeric take 8 mkString) ~
      dataPath.read[Feature] ~
      readyPath.read[Boolean]) (FeatureEntry.apply[Feature] _)
  private  val writes   : OWrites[FeatureEntry[Feature]] =
    (idPath.write[String] ~ readyPath.write[Boolean] ~ dataPath.write[Feature]) (fe => (fe._id, fe.ready, fe.data))
  implicit val format   : OFormat[FeatureEntry[Feature]] = OFormat(reads, writes)
}
