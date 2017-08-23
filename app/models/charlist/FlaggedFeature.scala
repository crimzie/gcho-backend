package models
package charlist

import com.github.dwickern.macros.NameOf._
import models.charlist.Charlist._
import play.api.libs.json.{Json, OWrites}

import scala.language.postfixOps

trait FlaggedFeature[T, This] {
  val _id: String
  val ready: Boolean
  val data: T
  var category: String
  var name: String
  val user: String

  def updated(user: String): This
}

object FlaggedFeature {
  val DEF_USER_VAL: String = "default"
  val ID: String = nameOf[FlaggedFeature[_, _]](_._id)
  val READY: String = nameOf[FlaggedFeature[_, _]](_.ready)
  val DATA: String = nameOf[FlaggedFeature[_, _]](_.data)
  val CAT: String = nameOf[FlaggedFeature[_, _]](_.category)
  val NAME: String = nameOf[FlaggedFeature[_, _]](_.name)
  val USER: String = nameOf[FlaggedFeature[_, _]](_.user)
  val TRAITS: String = "traits"
  val SKILLS: String = "skills"
  val TECHNIQUES: String = "tecns"
  val ARMOR: String = "armors"
  val WEAPONS: String = "weaps"
  val ITEMS: String = "items"
  implicit val flaggedFeatureWrites: OWrites[FlaggedFeature[_, _]] = OWrites {
    case ff: FlaggedTrait => Json.toJsObject(ff)(flaggedTraitFormat)
    case ff: FlaggedSkill => Json.toJsObject(ff)(flaggedSkillFormat)
    case ff: FlaggedTechnique => Json.toJsObject(ff)(flaggedTechniqueFormat)
    case ff: FlaggedWeapon => Json.toJsObject(ff)(flaggedWeaponFormat)
    case ff: FlaggedArmor => Json.toJsObject(ff)(flaggedArmorFormat)
    case ff: FlaggedItem => Json.toJsObject(ff)(flaggedItemFormat)
  }
}
