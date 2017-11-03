package controllers

import com.mohiva.play.silhouette.api.Silhouette
import daos.CharlistFeatureDao
import io.swagger.annotations._
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

  @ApiOperation(value = "Feature save", code = 202)
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Feature to be stored (for possible 'Feature' models see: Armor, Item, Skill, Technique, Trait, Weapon).",
      required = true,
      dataType = "models.charlist.FeatureEntry",
      paramType = "body"),
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid data structure"),
    new ApiResponse(code = 401, message = "Invalid JWT token")))
  def add(): Action[FeatureEntry[Feature]] =
    (silhouette.SecuredAction async parse.json[FeatureEntry[Feature]]) { request =>
      charlisFeatureDao save request.body.copy(request.identity._id) map (_ => Accepted)
    }

  @ApiOperation(value = "Feature retrieval", code = 200, response = classOf[Feature])
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Feature not found")))
  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlisFeatureDao.find(request.identity._id, id) map {
      case Some(jo) => Ok(jo)
      case _        => NotFound
    }
  }

  @ApiOperation(value = "Feature search", code = 200, responseContainer = "List")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header"),
    new ApiImplicitParam(
      name = "term",
      value = "search query",
      required = false,
      dataType = "string",
      paramType = "query")))
  @ApiResponses(value = Array(new ApiResponse(code = 401, message = "Invalid JWT token")))
  def search(cats: Seq[String], term: Option[String]): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlisFeatureDao find(request.identity._id, cats.toSet, term) map (Ok apply JsArray(_))
  }

  @ApiOperation(value = "Feature lookup", code = 200, responseContainer = "List")
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header"),
    new ApiImplicitParam(
      name = "term",
      value = "search query",
      required = false,
      dataType = "string",
      paramType = "query")))
  @ApiResponses(value = Array(new ApiResponse(code = 401, message = "Invalid JWT token")))
  def lookup(
      @ApiParam(allowableValues = "traits,skills,tecns,armors,weaps,items") cat: String,
      term: Option[String]): Action[AnyContent] = search(cat :: Nil, term)

  @ApiOperation(value = "Feature templates", code = 202, response = classOf[Feature])
  @ApiResponses(value = Array(new ApiResponse(code = 400, message = "Unknown feature type")))
  def create(@ApiParam(allowableValues = "traits,skills,tecns,armors,weaps,items") col: String): Action[AnyContent] =
    Action async Future(col match {
      case TRAITS     => Created(Json toJson Trait("New Trait"))
      case SKILLS     => Created(Json toJson Skill("New Skill"))
      case TECHNIQUES => Created(Json toJson Technique("New Technique", "Some Skill"))
      case WEAPONS    => Created(Json toJson Weapon("New Weapon"))
      case ARMOR      => Created(Json toJson Armor("New Armor"))
      case ITEMS      => Created(Json toJson Item("New Item"))
      case _          => BadRequest
    })
}
