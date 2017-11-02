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

  @ApiOperation(value = "Whole charlist save", code = 202)
  @ApiImplicitParams(Array(
    new ApiImplicitParam(
      name = "body",
      value = "Saved charlist (can overwrite existing if player and charlist ids are the same).",
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

  def list(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.all(request.identity._id).map(Ok apply JsArray(_))
  }

  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.find(request.identity._id, id) map (_.fold[Result](NotFound)(Ok.apply))
  }

  def create(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    val cl = Charlist.defCharlist.copy(
      _id = Charlist.randomId(),
      timestamp = DateTime.now.getMillis,
      player = request.identity._id)
    charlistDao save cl map { _ => Created(Json toJson cl) }
  }

  def replace(id: String): Action[Charlist] = (silhouette.SecuredAction async parse.json[Charlist]) { request =>
    val c = request.body.calc()
    val cl = if (c.player == request.identity._id && c._id == id) c else c.copy(_id = id, player = request.identity._id)
    charlistDao save cl map { _ => Accepted(Json toJson cl) }
  }

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

  def delete(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao exists(request.identity._id, id) flatMap {
      if (_) for {
        _ <- charlistDao delete(request.identity._id, id)
        _ <- picDao delete id
      } yield Ok else Future successful NotFound
    }
  }

  def storePic(id: String): Action[MultipartFormData[TemporaryFile]] =
    (silhouette.SecuredAction async parse.multipartFormData) { request =>
      (request.body file "pic" fold Future.successful[Result](BadRequest)) { p =>
        charlistDao exists(request.identity._id, id) flatMap {
          if (_) picDao save(id, Image fromPath p.ref.path cover(120, 150) bytes) map (_ => Accepted)
          else Future successful NotFound
        }
      }
    }

  def getPic(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao exists(request.identity._id, id) flatMap {
      if (_) picDao load id map {
        case Some(a) => Ok sendEntity HttpEntity.Strict(ByteString(a), Some("image/png"))
        case _       => NotFound
      } else Future successful NotFound
    }
  }
}
