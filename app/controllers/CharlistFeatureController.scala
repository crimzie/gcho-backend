package controllers

import com.mohiva.play.silhouette.api.Silhouette
import daos.CharlistFeatureDao
import io.swagger.annotations.Api
import models.auth.JWTEnv
import models.charlist.FeatureEntry._
import models.charlist._
import models.charlist.`trait`.Trait
import models.charlist.armor.Armor
import models.charlist.equipment.Item
import models.charlist.skill.{Skill, Technique}
import models.charlist.weapon.Weapon
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@Api("Charlist Features")
class CharlistFeatureController(
    components: ControllerComponents,
    silhouette: Silhouette[JWTEnv],
    charlisFeatureDao: CharlistFeatureDao)(implicit ec: ExecutionContext) extends AbstractController(components) {
  scribe debug "Instantiating."

  def add(): Action[FeatureEntry[Feature]] =
    (silhouette.SecuredAction async parse.json[FeatureEntry[Feature]]) { request =>
      charlisFeatureDao save request.body.copy(request.identity._id) map (_ => Accepted)
    }

  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlisFeatureDao.find(request.identity._id, id) map {
      case Some(jo) => Ok(jo)
      case _        => NotFound
    }
  }

  def lookup(cats: Seq[String], term: Option[String]): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlisFeatureDao find(request.identity._id, cats.toSet, term) map (Ok apply JsArray(_))
  }

  def create(col: String): Action[AnyContent] = Action async Future(col match {
    case TRAITS     => Created(Json toJson Trait("New Trait"))
    case SKILLS     => Created(Json toJson Skill("New Skill"))
    case TECHNIQUES => Created(Json toJson Technique("New Technique", "Some Skill"))
    case WEAPONS    => Created(Json toJson Weapon("New Weapon"))
    case ARMOR      => Created(Json toJson Armor("New Armor"))
    case ITEMS      => Created(Json toJson Item("New Item"))
    case _          => BadRequest
  })
}
