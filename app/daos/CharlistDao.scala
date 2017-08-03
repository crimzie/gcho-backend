package daos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import models.charlist.Charlist
import models.charlist.Charlist.charlistFormat
import models.charlist.CharlistFields._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json
import play.modules.reactivemongo.ReactiveMongoApi
import play.modules.reactivemongo.json._
import reactivemongo.api.Cursor
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.Future

@ImplementedBy(classOf[MongoCharlistDao])
trait CharlistDao {
  def save(charlist: Charlist): Future[Unit]

  def find(): Future[Seq[Charlist]]

  def find(id: String): Future[Option[Charlist]]

  def exists(id: String): Future[Boolean]

  def update(charlist: Charlist): Future[Unit]

  def delete(id: String): Future[Unit]
}

@Singleton
class MongoCharlistDao @Inject()(mongo: ReactiveMongoApi) extends CharlistDao {
  val storage: Future[JSONCollection] = mongo.database map (_ collection[JSONCollection] "characters")

  override def save(charlist: Charlist): Future[Unit] = for {
    collection <- storage
    _ <- collection insert charlist
  } yield ()

  override def find(): Future[Seq[Charlist]] = for {
    collection <- storage
    charlists <- collection.find().cursor[Charlist]().collect(-1, Cursor.FailOnError[Seq[Charlist]]())
  } yield charlists // map (c => Json obj (ID -> c._id, TIMESTAMP -> c.timestamp, PLAYER -> c.player, NAME -> c.name))

  override def find(id: String): Future[Option[Charlist]] = for {
    collection <- storage
    charlist <- collection.find(Json obj ID -> id).one[Charlist]
  } yield charlist

  override def exists(id: String): Future[Boolean] = for {
    collection <- storage
    result <- collection count Some(Json obj ID -> id)
  } yield result > 0

  override def update(charlist: Charlist): Future[Unit] = for {
    collection <- storage
    _ <- collection update(Json obj ID -> charlist._id, charlist, upsert = true)
  } yield ()

  override def delete(id: String): Future[Unit] = for {
    collection <- storage
    _ <- collection.remove(Json obj ID -> id)
  } yield ()
}
