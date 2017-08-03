import javax.inject.Inject

import filters.{LoggingFilter, NoriginFilter}
import play.api.http.HttpFilters
import play.api.mvc.EssentialFilter

class Filters @Inject()(corsFilter: NoriginFilter, log: LoggingFilter) extends HttpFilters {
  def filters = Seq[EssentialFilter](corsFilter, log)
}
