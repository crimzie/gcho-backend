package controllers

import akka.util.ByteString
import com.google.inject.Inject
import com.sksamuel.scrimage.Image
import com.sksamuel.scrimage.nio.PngWriter
import daos.{CharlistDao, PicDao}
import models.charlist.Charlist._
import models.charlist._
import play.api.Configuration
import play.api.http.HttpEntity
import play.api.libs.Files.TemporaryFile
import play.api.libs.json._
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class CharlistController @Inject()(charlistDao: CharlistDao, picDao: PicDao, configuration: Configuration)
                                  (implicit ec: ExecutionContext) extends InjectedController {
  implicit val pw: PngWriter = PngWriter.MinCompression

  def add(): Action[Charlist] = Action.async(parse.json[Charlist]) { implicit request =>
    charlistDao save request.body map { _ => Accepted(Json toJson request.body) }
  }

  def list: Action[AnyContent] = Action.async {
    charlistDao all "" map {
      Ok apply Json.toJson(_)
    }
  }

  def get(id: String): Action[AnyContent] = Action.async {
    charlistDao find("", id) map {
      case Some(cl) => Ok(Json toJson cl)
      case None => NotFound
    }
  }

  def create(p: String): Action[AnyContent] = Action async {
    val cl = Charlist()
    charlistDao save cl map { _ => Created(Json toJson cl) }
  }

  def replace(): Action[Charlist] = Action.async(parse.json[Charlist]) { implicit request =>
    val cl = request.body.copy(player = "")
    charlistDao update cl map { _ => Accepted(Json toJson cl) }
  }

  def update(id: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    for {
      j <- charlistDao find("", id)
      r <- j.asInstanceOf[JsObject].deepMerge(request.body.asInstanceOf[JsObject]).validate[Charlist] match {
        case JsSuccess(cl, _) => charlistDao save cl.copy(_id = id, player = "") map (_ => Accepted)
        case JsError(ers) => Future successful BadRequest((JsObject.empty /: ers) {
          case (jo, (jp, seq)) => jo deepMerge jp.write[JsArray].writes(JsArray apply seq.map(Json toJson _.message))
        })
      }
    } yield r
  }

  def delete(id: String): Action[AnyContent] = Action.async(for {
    b <- charlistDao exists("", id)
    r <- if (b) for {
      _ <- charlistDao delete("", id)
      _ <- picDao delete id
    } yield Ok else Future successful NotFound
  } yield r)

  def storePic(id: String): Action[MultipartFormData[TemporaryFile]] =
    Action.async(parse.multipartFormData) { implicit request =>
      request.body file "pic" match {
        case None => Future successful BadRequest("Missing file.")
        case Some(p) => for {
          b <- charlistDao exists("", id)
          r <- if (b) picDao save(id, Image fromPath p.ref.path cover(120, 150) bytes) map (_ => Accepted)
          else Future successful NotFound("Charlist doesn't exist.")
        } yield r
      }
    }

  def getPic(id: String): Action[AnyContent] = Action.async(for {
    b <- charlistDao exists("", id)
    r <- if (b) picDao load id map {
      case None => NotFound
      case Some(a) => Ok sendEntity HttpEntity.Strict(ByteString(a), Some("image/png"))
    } else Future successful NotFound("Charlist doesn't exist.")
  } yield r)
}
