package daos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

@ImplementedBy(classOf[MongoPicDao])
trait PicDao {
  def save(id: String, pic: Array[Byte]): Future[Unit]

  def load(id: String): Future[Option[Array[Byte]]]

  def delete(id: String): Future[Unit]
}

@Singleton
class MongoPicDao @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) extends PicDao {
  private val storage: Future[BSONCollection] = mongo.database map (_ collection[BSONCollection] "pics")
  private val byId: String => BSONDocument = id => BSONDocument("_id" -> id)

  override def save(id: String, pic: Array[Byte]): Future[Unit] = for {
    c <- storage
    _ <- c update(byId(id), BSONDocument("_id" -> id, "pic" -> pic), upsert = true)
  } yield ()

  override def load(id: String): Future[Option[Array[Byte]]] = for {
    c <- storage
    optD <- c.find(byId(id)).one[BSONDocument]
  } yield for {
    d <- optD
    a <- d.getAs[Array[Byte]]("pic")
  } yield a

  override def delete(id: String): Future[Unit] = for {
    c <- storage
    _ <- c remove byId(id)
  } yield ()
}
