package controllers

import akka.util.ByteString
import com.mohiva.play.silhouette.api.Silhouette
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter
import daos.{CharlistDao, PicDao}
import io.swagger.annotations._
import models.auth.JWTEnv
import models.charlist.Charlist._
import models.charlist._
import org.joda.time.DateTime
import play.api.http.HttpEntity
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@Api("Charlists")
class CharlistController(
    components: ControllerComponents,
    silhouette: Silhouette[JWTEnv],
    charlistDao: CharlistDao,
    picDao: PicDao)(implicit ec: ExecutionContext) extends AbstractController(components) {
  scribe debug "Instantiating."
  implicit val pw: PngWriter = PngWriter.MinCompression

  @ApiOperation(value = "Whole charlist save", code = 202, response = classOf[Charlist])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Charlist to be stored (can overwrite existing if player and charlist ids are the same).",
      required = true,
      dataType = "models.charlist.Charlist",
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
  def add(): Action[Charlist] = (silhouette.SecuredAction async parse.json[Charlist]) { request =>
    val c = request.body.calc()
    val cl = if (c.player == request.identity._id) c else c.copy(player = request.identity._id)
    charlistDao save cl map { _ => Accepted(Json toJson cl) }
  }

  @ApiOperation(value = "Characters listing", code = 200, responseContainer = "List")
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(new ApiResponse(code = 401, message = "Invalid JWT token")))
  def list(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.all(request.identity._id).map(Ok apply JsArray(_))
  }

  @ApiOperation(value = "Charlist retrieval", code = 200, response = classOf[Charlist])
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Id not found")))
  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.find(request.identity._id, id) map (_.fold[Result](NotFound)(Ok.apply))
  }

  @ApiOperation(value = "New charlist", code = 201, response = classOf[Charlist])
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(new ApiResponse(code = 401, message = "Invalid JWT token")))
  def create(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    val cl = Charlist.defCharlist.copy(
      _id = Charlist.randomId(),
      timestamp = DateTime.now.getMillis,
      player = request.identity._id)
    charlistDao save cl map { _ => Created(Json toJson cl) }
  }

  @ApiOperation(value = "Storing or replacing charlist", code = 202, response = classOf[Charlist])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header"),
    new ApiImplicitParam(
      name = "body",
      value = "Charlist to be stored.",
      required = true,
      dataType = "models.charlist.Charlist",
      paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid input charlist JSON"),
    new ApiResponse(code = 401, message = "Invalid JWT token")))
  def replace(id: String): Action[Charlist] = (silhouette.SecuredAction async parse.json[Charlist]) { request =>
    val c = request.body.calc()
    val cl = if (c.player == request.identity._id && c._id == id) c else c.copy(_id = id, player = request.identity._id)
    charlistDao save cl map { _ => Accepted(Json toJson cl) }
  }

  @ApiOperation(value = "Charlist update", code = 202, response = classOf[Charlist])
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header"),
    new ApiImplicitParam(
      name = "body",
      value = "Charlist update.",
      required = true,
      dataType = "models.charlist.Charlist",
      paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid input JSON"),
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Charlist not found")))
  def update(id: String): Action[JsObject] = (silhouette.SecuredAction async parse.json[JsObject]) { request =>
    charlistDao find(request.identity._id, id) flatMap {
      case None    => Future successful NotFound
      case Some(j) => j.deepMerge(request.body).validate[Charlist] match {
        case JsError(ers)         => Future successful BadRequest((JsObject.empty /: ers) {
          case (jo, (jp, seq)) => jo deepMerge jp.write[JsArray].writes(JsArray apply seq.map(Json toJson _.message))
        })
        case JsSuccess(merged, _) =>
          val cl = if (merged.player == request.identity._id && merged._id == id)
            merged.calc() else merged.calc().copy(_id = id, player = request.identity._id)
          charlistDao save cl map { _ => Accepted(Json toJson cl) }
      }
    }
  }

  @ApiOperation(value = "Charlist deletion", code = 204)
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Charlist not found")))
  def delete(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao exists(request.identity._id, id) flatMap {
      if (_) for {
        _ <- charlistDao delete(request.identity._id, id)
        _ <- picDao delete id
      } yield NoContent else Future successful NotFound
    }
  }

  @ApiOperation(value = "Charlist portrait storing", code = 202)
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "X-Auth-Token",
      value = "JWT session token",
      required = true,
      dataType = "string",
      paramType = "header"),
    new ApiImplicitParam(
      name = "pic",
      value = "Charlist portrait",
      required = true,
      `type` = "file",
      paramType = "form")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "No 'pic' file in request"),
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Id not found")))
  def storePic(id: String): Action[MultipartFormData[TemporaryFile]] =
    (silhouette.SecuredAction async parse.multipartFormData) { request =>
      (request.body file "pic" fold Future.successful[Result](BadRequest)) { p =>
        charlistDao exists(request.identity._id, id) flatMap {
          if (_) picDao save(id, Image fromPath p.ref.path cover(120, 150) bytes) map (_ => Accepted)
          else Future successful NotFound
        }
      }
    }

  @ApiOperation(value = "Charlist portrait retrieval", code = 200, produces = "image/png")
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 401, message = "Invalid JWT token"),
    new ApiResponse(code = 404, message = "Id or portrait not found")))
  def getPic(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao exists(request.identity._id, id) flatMap {
      if (_) picDao load id map {
        case Some(a) => Ok sendEntity HttpEntity.Strict(ByteString(a), Some("image/png"))
        case _       => NotFound
      } else Future successful NotFound
    }
  }
}
