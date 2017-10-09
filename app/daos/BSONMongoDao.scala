package daos

import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.{ExecutionContext, Future}

trait BSONMongoDao {
  implicit val ec: ExecutionContext
  val mongo: Future[DefaultDB]
  val colName: String
  lazy val storage: Future[BSONCollection] = mongo map (_ collection[BSONCollection] colName)
  val d: BSONDocument = BSONDocument.empty
}
