package daos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import models.charlist.Charlist
import models.charlist.Charlist._
import play.api.libs.json.Json.obj
import play.modules.reactivemongo.ReactiveMongoApi
import play.modules.reactivemongo.json._
import reactivemongo.api.Cursor
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.Ascending
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[MongoCharlistDao])
trait CharlistDao {
  def save(charlist: Charlist): Future[Unit]

  def all(user: String): Future[Seq[Charlist]]

  def find(user: String, id: String): Future[Option[Charlist]]

  def exists(user: String, id: String): Future[Boolean]

  def delete(user: String, id: String): Future[Unit]
}

@Singleton
class MongoCharlistDao @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext) extends CharlistDao {
  val storage: Future[JSONCollection] = mongo.database map (_ collection[JSONCollection] "characters")
  storage map (_.indexesManager ensure Index(PLAYER -> Ascending :: Nil, name = Some(s"bin-$PLAYER-1")))

  override def save(charlist: Charlist): Future[Unit] = for {
    collection <- storage
    _ <- collection update(obj(ID -> charlist._id, PLAYER -> charlist.player), charlist, upsert = true)
  } yield ()

  override def all(user: String): Future[Seq[Charlist]] = for {
    collection <- storage
    charlists <- collection
      .find(obj(PLAYER -> user))
      .cursor[Charlist]()
      .collect(-1, Cursor.FailOnError[Seq[Charlist]]())
  } yield charlists

  override def find(user: String, id: String): Future[Option[Charlist]] = for {
    collection <- storage
    charlist <- collection.find(obj(ID -> id, PLAYER -> user)).one[Charlist]
  } yield charlist

  override def exists(user: String, id: String): Future[Boolean] = for {
    collection <- storage
    result <- collection count Some(obj(ID -> id, PLAYER -> user))
  } yield result > 0

  override def delete(user: String, id: String): Future[Unit] = for {
    collection <- storage
    _ <- collection remove obj(ID -> id, PLAYER -> user)
  } yield ()
}
