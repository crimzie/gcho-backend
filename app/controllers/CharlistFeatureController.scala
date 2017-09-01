package controllers

import com.google.inject.Inject
import com.mohiva.play.silhouette.api.Silhouette
import daos.CharlistFeatureDao
import models.auth.JWTEnv
import models.charlist.Charlist._
import models.charlist.FlaggedFeature._
import models.charlist._
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Success, Try}

class CharlistFeatureController @Inject()(silhouette: Silhouette[JWTEnv], charlisFeatureDao: CharlistFeatureDao)
                                         (implicit ec: ExecutionContext) extends InjectedController {

  def add(col: String): Action[JsValue] = (silhouette.SecuredAction async parse.json) { request =>
    Try {
      request.body.validate(col match {
        case ARMOR => flaggedArmorFormat
        case WEAPONS => flaggedWeaponFormat
        case ITEMS => flaggedItemFormat
        case TRAITS => flaggedTraitFormat
        case SKILLS => flaggedSkillFormat
        case TECHNIQUES => flaggedTechniqueFormat
      })
    } match {
      case Success(JsSuccess(ff: FlaggedFeature[_, _], _)) =>
        charlisFeatureDao save ff.updated(request.identity._id).asInstanceOf[FlaggedFeature[_, _]] map (_ => Accepted)
      case Success(JsError(ers)) => Future successful BadRequest((JsObject.empty /: ers) {
        case (jo, (jp, seq)) => jo deepMerge jp.write[JsArray].writes(JsArray apply seq.map(Json toJson _.message))
      })
      case _ => Future successful BadRequest
    }
  }

  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlisFeatureDao.find(request.identity._id, id) map {
      case Some(jo) => Ok(jo)
      case _ => NotFound
    }
  }

  def lookup(category: String, term: Option[String]): Action[AnyContent] = silhouette.SecuredAction async { request =>
    val cat: Set[String] = (category toLowerCase() split ',' toSet) - ""
    charlisFeatureDao find(request.identity._id, cat, term) map (Ok apply JsArray(_))
  }

  def create(col: String): Action[AnyContent] = Action async Future(col match {
    case TRAITS => Created(Json toJson Trait())
    case SKILLS => Created(Json toJson Skill())
    case TECHNIQUES => Created(Json toJson Technique())
    case WEAPONS => Created(Json toJson Weapon())
    case ARMOR => Created(Json toJson Armor())
    case ITEMS => Created(Json toJson Item())
    case _ => BadRequest
  })
}
