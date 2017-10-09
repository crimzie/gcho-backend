package controllers

import play.api.cache.SyncCacheApi
import play.api.mvc._
import play.api.routing.Router

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class MiscController(components: ControllerComponents, cacheApi: SyncCacheApi, router: => Router)
                    (implicit ec: ExecutionContext) extends AbstractController(components) {
  scribe debug "Instantiating."
  val methods: List[String] = "GET" :: "POST" :: "PUT" :: "DELETE" :: "PATCH" :: Nil

  def options(url: String): Action[AnyContent] = Action async { request =>
    Future {
      val ms = (cacheApi getOrElseUpdate "options.url." + request.uri) {
        for {
          m <- methods
          if router handlerFor (request withMethod m) isDefined
        } yield m
      }
      if (ms.nonEmpty) {
        val str = "OPTIONS" :: ms mkString ", "
        NoContent withHeaders(
          ALLOW -> str,
          ACCESS_CONTROL_ALLOW_METHODS -> str,
          ACCESS_CONTROL_ALLOW_HEADERS -> (request.headers get ACCESS_CONTROL_REQUEST_HEADERS getOrElse ""))
      } else NotFound
    }
  }
}
