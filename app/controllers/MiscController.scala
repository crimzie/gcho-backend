package controllers

import com.google.inject.{Inject, Provider, Singleton}
import play.api.cache.SyncCacheApi
import play.api.mvc.{Action, AnyContent, InjectedController}
import play.api.routing.Router

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@Singleton
class MiscController @Inject()(cacheApi: SyncCacheApi, router: Provider[Router])(implicit ec: ExecutionContext)
  extends InjectedController {
  val methods: List[String] = "GET" :: "POST" :: "PUT" :: "DELETE" :: "PATCH" :: Nil

  def options(url: String): Action[AnyContent] = Action async { request =>
    Future {
      val ms = (cacheApi getOrElseUpdate "options.url." + request.uri) {
        for {
          m <- methods
          if router.get handlerFor (request withMethod m) isDefined
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
