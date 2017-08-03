package controllers

import com.google.inject.Inject
import daos.CharlistFeatureDao
import daos.MongoCollections._
import models.charlist.Charlist._
import models.charlist._
import org.mongodb.scala.Completed
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, Controller, Result}

import scala.concurrent.Future

/**
  * Created by crimson on 11/16/16.
  */
class CharlistFeatureController @Inject()(charlisFeatureDao: CharlistFeatureDao) extends Controller {
  private val invalidMsg = { e: JsError =>
    Future(BadRequest(Json toJson (e.errors map { case (a, b) => Json obj a.toString -> b.mkString("; ") })))
  }
  private val throwMsg: PartialFunction[Throwable, Future[Result]] = {
    case e: IllegalStateException => Future(NotFound(Json obj "Empty database return." -> e.toString))
    case t: Throwable => Future(InternalServerError(t.toString))
  }

  def options(p: String, name: String): Action[AnyContent] = Action { implicit request =>
    val methods = p match {
      case "base" => "GET, POST"
      case "list" => "GET"
      case "elem" => "GET"
    }
    val requestHeaders = request.headers get ACCESS_CONTROL_REQUEST_HEADERS getOrElse ""
    Ok withHeaders(
      ALLOW -> methods,
      ACCESS_CONTROL_ALLOW_METHODS -> methods,
      ACCESS_CONTROL_ALLOW_HEADERS -> requestHeaders)
  }

  def add(col: String): Action[JsValue] = Action.async(parse.json) { implicit request =>
    def f[A <: FlaggedFeature](oFormat: OFormat[A]) = request.body.validate[A](oFormat) match {
      case e: JsError => invalidMsg(e)
      case s: JsSuccess[A] =>
        charlisFeatureDao save s.get map { _: Completed => Accepted(Json.toJson(s.get)(oFormat)) } recoverWith throwMsg
    }

    col match {
      case TRAITS => f(flaggedTraitFormat)
      case SKILLS => f(flaggedSkillFormat)
      case TECHNIQUES => f(flaggedTechniqueFormat)
      case WEAPONS => f(flaggedWeaponFormat)
      case ARMORS => f(flaggedArmorFormat)
      case ITEMS => f(flaggedItemFormat)
    }
  }

  def get(col: String, id: String): Action[AnyContent] =
    Action async (charlisFeatureDao find(col, id) map { t: JsValue => Ok(t) } recoverWith throwMsg)

  def lookup(col: String, category: String, term: String): Action[AnyContent] = Action.async {
    val seq: Seq[String] = if (category == "") Nil else category toLowerCase() split ',' map (_.capitalize)
    (if (term == "") charlisFeatureDao find(col, seq) else charlisFeatureDao find(col, seq, term)) map {
      s: Seq[JsObject] => Ok(Json toJson s)
    } recoverWith throwMsg
  }

  def create(col: String): Action[AnyContent] = Action async Future(Created(col match {
    case TRAITS => Json toJson Trait()
    case SKILLS => Json toJson Skill()
    case TECHNIQUES => Json toJson Technique()
    case WEAPONS => Json toJson Weapon()
    case ARMORS => Json toJson Armor()
    case ITEMS => Json toJson Item()
  }))
}
