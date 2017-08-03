package filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.Logger
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

class LoggingFilter @Inject()(implicit val mat: Materializer, ec: ExecutionContext) extends Filter {

  def apply(nextFilter: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    val startTime = System.currentTimeMillis
    nextFilter(rh).map { result =>
      val requestTime = System.currentTimeMillis - startTime
      Logger.info(s"${rh.method} ${rh.uri} took ${requestTime}ms and returned ${result.header.status}")
      result.withHeaders("Request-Time" -> requestTime.toString)
    }
  }
}
