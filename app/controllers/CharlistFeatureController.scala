package controllers

import com.google.inject.Inject
import daos.CharlistFeatureDao
import models.charlist.Charlist._
import models.charlist.FlaggedFeature._
import models.charlist._
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class CharlistFeatureController @Inject()(charlisFeatureDao: CharlistFeatureDao)(implicit ec: ExecutionContext)
  extends InjectedController {

  def add(col: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body.validate(col match {
      case ARMOR => flaggedArmorFormat
      case WEAPONS => flaggedWeaponFormat
      case ITEMS => flaggedItemFormat
      case TRAITS => flaggedTraitFormat
      case SKILLS => flaggedSkillFormat
      case TECHNIQUES => flaggedTechniqueFormat
    }) match {
      case JsError(ers) => Future successful BadRequest((JsObject.empty /: ers) {
        case (jo, (jp, seq)) => jo deepMerge jp.write[JsArray].writes(JsArray apply seq.map(Json toJson _.message))
      })
      case JsSuccess(value: FlaggedFeature[_, _], _) => charlisFeatureDao save value map (_ => Accepted)
    }
  }

  def get(id: String): Action[AnyContent] = Action.async(charlisFeatureDao find("", id) map {
    case Some(jo) => Ok(jo)
    case None => NotFound
  })

  def lookup(category: String, term: Option[String]): Action[AnyContent] = Action.async {
    val cat: Set[String] = (category toLowerCase() split ',' toSet) - ""
    charlisFeatureDao find("", cat, term) map (Ok apply JsArray(_))
  }

  def create(col: String): Action[AnyContent] = Action async Future(Created(col match {
    case TRAITS => Json toJson Trait()
    case SKILLS => Json toJson Skill()
    case TECHNIQUES => Json toJson Technique()
    case WEAPONS => Json toJson Weapon()
    case ARMOR => Json toJson Armor()
    case ITEMS => Json toJson Item()
  }))
}
