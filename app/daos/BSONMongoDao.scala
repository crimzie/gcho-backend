package daos

import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.{ExecutionContext, Future}

trait BSONMongoDao {
  implicit val ec: ExecutionContext
  val mongo: ReactiveMongoApi
  val colName: String
  val storage: Future[BSONCollection] = mongo.database map (_ collection[BSONCollection] colName)
  val d: BSONDocument = BSONDocument.empty
}
