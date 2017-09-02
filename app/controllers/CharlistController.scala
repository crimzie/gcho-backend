package controllers

import akka.util.ByteString
import com.google.inject.Inject
import com.mohiva.play.silhouette.api.Silhouette
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter
import daos.{CharlistDao, PicDao}
import models.auth.JWTEnv
import models.charlist.Charlist._
import models.charlist._
import play.api.Configuration
import play.api.http.HttpEntity
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class CharlistController @Inject()(
                                    silhouette: Silhouette[JWTEnv],
                                    charlistDao: CharlistDao,
                                    picDao: PicDao,
                                    configuration: Configuration)
                                  (implicit ec: ExecutionContext) extends InjectedController {
  implicit val pw: PngWriter = PngWriter.MinCompression

  def add(): Action[Charlist] = (silhouette.SecuredAction async parse.json[Charlist]) { request =>
    val cl = if (request.body.player == request.identity._id) request.body
    else request.body.copy(player = request.identity._id)
    charlistDao save cl map { _ => Accepted(Json toJson cl) }
  }

  def list(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.all(request.identity._id).map(Ok apply JsArray(_))
  }

  def get(id: String): Action[AnyContent] = silhouette.SecuredAction async { request =>
    charlistDao.find(request.identity._id, id) map (_.fold[Result](NotFound)(Ok.apply))
  }

  def create(): Action[AnyContent] = silhouette.SecuredAction async { request =>
    val cl = Charlist(player = request.identity._id)
    charlistDao save cl map { _ => Created(Json toJson cl) }
  }

  def replace(id: String): Action[Charlist] = (silhouette.SecuredAction async parse.json[Charlist]) { request =>
    val cl = if (request.body.player == request.identity._id && request.body._id == id) request.body
    else request.body.copy(_id = id, player = request.identity._id)
    charlistDao save cl map { _ => Accepted(Json toJson cl) }
  }

  def update(id: String): Action[JsValue] = (silhouette.SecuredAction async parse.json) { request =>
    charlistDao find(request.identity._id, id) flatMap {
      case Some(j) => j.deepMerge(request.body.asInstanceOf[JsObject]).validate[Charlist] match {
        case JsSuccess(cl, _) =>
          val c = cl.copy(_id = id, player = request.identity._id)
          charlistDao save c map { _ => Accepted(Json toJson cl) }
        case JsError(ers) => Future successful BadRequest((JsObject.empty /: ers) {
          case (jo, (jp, seq)) => jo deepMerge jp.write[JsArray].writes(JsArray apply seq.map(Json toJson _.message))
        })
      }
      case _ => Future successful NotFound
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
        case _ => NotFound
      } else Future successful NotFound
    }
  }
}
