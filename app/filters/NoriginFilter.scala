package filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.http.{DefaultHttpErrorHandler, HeaderNames, HttpErrorHandler}
import play.api.mvc.{Filter, RequestHeader, Result}

import scala.concurrent.Future

/**
  * Created by crimson on 10/23/16.
  */
class NoriginFilter @Inject()(protected val errorHandler: HttpErrorHandler = DefaultHttpErrorHandler)
                             (override implicit val mat: Materializer) extends Filter {

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] = {
    val headerBuilder = Seq.newBuilder[(String, String)]
    headerBuilder += HeaderNames.ACCESS_CONTROL_ALLOW_ORIGIN -> "*"
    import play.api.libs.iteratee.Execution.Implicits.trampoline
    val result = try {
      f(rh).recoverWith { case e: Throwable => errorHandler.onServerError(rh, e) }
    } catch {
      case e: Throwable => errorHandler.onServerError(rh, e)
    }
    result.map { _.withHeaders(headerBuilder.result(): _*) }
  }
}
