package controllers

import java.io.File

import com.google.inject.Inject
import com.sksamuel.scrimage.Image
import daos.CharlistDao
import models.charlist.Charlist._
import models.charlist._
import org.mongodb.scala.Completed
import org.mongodb.scala.result.UpdateResult
import play.api.Configuration
import play.api.libs.Files.TemporaryFile
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc._
import scala.concurrent.Future
import scala.util.Random

/**
  * Created by crimson on 9/23/16.
  */
class CharlistController @Inject()(charlistDao: CharlistDao, configuration: Configuration) extends Controller {
  private val invalidMsg = { e: JsError =>
    Future(BadRequest(Json toJson (e.errors map { case (a, b) => Json obj a.toString -> b.mkString("; ") })))
  }
  private val throwMsg: PartialFunction[Throwable, Future[Result]] = {
    case e: IllegalStateException => Future(NotFound(Json obj "Empty database return." -> e.toString))
    case t: Throwable => Future(InternalServerError(t.toString))
  }
  private val picFile = { id: String => new File(s"${configuration.underlying getString "files.picfolder"}$id.png") }

  def options(p: String, id: String): Action[AnyContent] = Action { implicit request =>
    val methods = p match {
      case "base" => "GET"
      case "list" => "GET, POST"
      case "elem" => "GET, PUT, PATCH, DELETE"
      case "file" => "GET, PUT"
    }
    Ok withHeaders(
      ALLOW -> methods,
      ACCESS_CONTROL_ALLOW_METHODS -> methods,
      ACCESS_CONTROL_ALLOW_HEADERS -> (request.headers get ACCESS_CONTROL_REQUEST_HEADERS getOrElse ""))
  }

  def add(): Action[JsValue] = Action.async(parse.json) { implicit request =>
    request.body.validate[Charlist] match {
      case e: JsError => invalidMsg(e)
      case s: JsSuccess[Charlist] =>
        val charlist = s.get copy(
          _id = math abs Random.nextLong toString(),
          timestamp = System.currentTimeMillis toString())
        charlistDao save charlist map { _ => Accepted(Json toJson charlist) } recoverWith throwMsg
    }
  }

  def list: Action[AnyContent] = Action.async {
    charlistDao find() map { list: Seq[JsObject] => Ok(Json toJson list) } recoverWith throwMsg
  }

  def get(id: String): Action[AnyContent] = Action.async {
    charlistDao find id map { charlist: JsValue => Ok(charlist) } recoverWith throwMsg
  }

  def create(p: String): Action[AnyContent] = Action async Future(Created(Json toJson Charlist()))

  def update(id: String, replace: Boolean): Action[JsValue] = Action.async(parse.json) { implicit request =>
    def save(ch: JsValue): Future[Result] = ch.validate[Charlist] match {
      case e: JsError => invalidMsg(e)
      case s: JsSuccess[Charlist] =>
        val charlist = s.get copy (_id = id)
        charlistDao update charlist map { _ => Accepted(Json toJson charlist) } recoverWith throwMsg
    }
    val b = request.body
    if (replace) save(b)
    else charlistDao find id flatMap { j => save(j.as[JsObject] deepMerge b.as[JsObject]) } recoverWith throwMsg
  }

  def delete(id: String): Action[AnyContent] = Action.async {
    picFile(id) delete()
    charlistDao delete id map { list => Ok(Json toJson list) } recoverWith throwMsg
  }

  def storePic(id: String): Action[MultipartFormData[TemporaryFile]] = Action.async(parse.multipartFormData) {
    implicit request =>
      charlistDao exists id map {
        if (_) request.body file "pic" map { p =>
          val pf = picFile(id)
          if (pf.exists) pf delete()
          Image fromFile p.ref.file cover(120, 150) output pf
        } match {
          case _: Some[File] => Accepted("Pic uploaded.")
          case None => BadRequest("Missing file.")
        } else NotFound("Charlist doesn't exist.")
      } recoverWith throwMsg
  }

  def getPic(id: String): Action[AnyContent] = Action {
    val pf = picFile(id)
    Ok sendFile(if (pf.exists) pf else new File(configuration.underlying getString "files.defaultpic"), inline = true)
  }
}
