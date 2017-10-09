package daos

import models.charlist.Charlist
import models.charlist.Charlist._
import play.api.libs.json.JsObject
import play.api.libs.json.Json.obj
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.Ascending
import reactivemongo.api.{Cursor, DefaultDB}
import reactivemongo.play.json._
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.{ExecutionContext, Future}

trait CharlistDao {
  def save(charlist: Charlist): Future[Unit]

  def all(user: String): Future[Seq[JsObject]]

  def find(user: String, id: String): Future[Option[JsObject]]

  def exists(user: String, id: String): Future[Boolean]

  def delete(user: String, id: String): Future[Unit]
}

class MongoCharlistDao(mongo: Future[DefaultDB])(implicit ec: ExecutionContext) extends CharlistDao {
  scribe debug "Instantiating."
  val storage: Future[JSONCollection] = mongo map (_ collection[JSONCollection] "characters")

  storage map (_.indexesManager ensure Index(PLAYER -> Ascending :: Nil, name = Some(s"bin-$PLAYER-1")))

  override def save(charlist: Charlist): Future[Unit] = for {
    collection <- storage
    _ <- collection update(obj(ID -> charlist._id, PLAYER -> charlist.player), charlist, upsert = true)
  } yield ()

  override def all(user: String): Future[Seq[JsObject]] = for {
    collection <- storage
    charlists <- collection
      .find(obj(PLAYER -> user))
      .projection(obj(NAME -> 1, TIMESTAMP -> 1))
      .cursor[JsObject]()
      .collect(-1, Cursor.FailOnError[Seq[JsObject]]())
  } yield charlists

  override def find(user: String, id: String): Future[Option[JsObject]] = for {
    collection <- storage
    charlist <- collection.find(obj(ID -> id, PLAYER -> user)).one[JsObject]
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
