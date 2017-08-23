import javax.inject.Inject

import filters.NoriginFilter
import play.api.http.HttpFilters
import play.api.mvc.EssentialFilter

class Filters @Inject()(corsFilter: NoriginFilter) extends HttpFilters {
  def filters: Seq[EssentialFilter] = corsFilter :: Nil
}
