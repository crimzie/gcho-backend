package filters

import akka.stream.Materializer
import play.api.http.HeaderNames
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.{ExecutionContext, Future}

class NoriginFilter(implicit val mat: Materializer, implicit val ec: ExecutionContext) extends Filter {
  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
    f(rh) map (_ withHeaders HeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN -> "*")
}
