package daos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import models.charlist.FlaggedFeature._
import models.charlist._
import play.api.Logger
import play.api.libs.json.JsObject
import play.api.libs.json.Json.{arr, obj}
import play.modules.reactivemongo.ReactiveMongoApi
import play.modules.reactivemongo.json._
import reactivemongo.api.Cursor
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.{Ascending, Text}
import reactivemongo.play.json.collection.JSONCollection
import services.defaults._

import scala.concurrent.duration.{Duration, MINUTES}
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

@ImplementedBy(classOf[MongoCharlistFeatureDao])
trait CharlistFeatureDao {
  def save(feature: FlaggedFeature[_, _]): Future[Unit]

  def remove(user: String, id: String): Future[Unit]

  def find(user: String, id: String): Future[Option[JsObject]]

  def find(user: String, cat: Set[String], term: Option[String] = None): Future[Seq[JsObject]]
}

@Singleton
class MongoCharlistFeatureDao @Inject()(mongo: ReactiveMongoApi)(implicit ec: ExecutionContext)
  extends CharlistFeatureDao {
  val logger: Logger = Logger(classOf[MongoCharlistFeatureDao])
  val storage: Future[JSONCollection] = mongo.database map (_ collection[JSONCollection] "features")
  storage map (_.indexesManager ensure Index(USER -> Ascending :: Nil, name = Some(s"bin-$USER-1")))
  storage map (_.indexesManager ensure Index(CAT -> Ascending :: Nil, name = Some(s"bin-$CAT-1")))
  storage map (_.indexesManager ensure Index(NAME -> Text :: Nil, name = Some(s"txt-$NAME")))

  /*private val defaultsPreloading: Future[Unit] = for {
    s <- storage
    _ <- s remove obj(USER -> DEF_USER_VAL)
    stream = DefaultTraits.parse("defaults/adv.xml") ++ DefaultSkills.parse("defaults/skl.xml") ++
      DefaultTechniques.parse("defaults/skl.xml") ++ DefaultArmor.parse("defaults/eqp.xml") ++
      DefaultWeapons.parse("defaults/eqp.xml") ++ DefaultItems.parse("defaults/eqp.xml")
    _ <- s bulkInsert(stream, ordered = false)
  } yield ()
  defaultsPreloading onComplete {
    case Success(_) => logger info "Default features collection loaded to db."
    case Failure(_) => logger warn "Failed to load default features collection to db."
  }
  Await.result(defaultsPreloading, Duration(3, MINUTES))*/

  override def save(feature: FlaggedFeature[_, _]): Future[Unit] = for {
    collection <- storage
    _ <- collection.update(obj(USER -> feature.user, ID -> feature._id), feature, upsert = true)
  } yield ()

  override def remove(user: String, id: String): Future[Unit] = for {
    collection <- storage
    _ <- collection remove obj(USER -> user, ID -> id)
  } yield ()

  override def find(user: String, id: String): Future[Option[JsObject]] = for {
    collection <- storage
    feature <- collection.find(obj(USER -> obj("$in" -> arr(user, DEF_USER_VAL)), ID -> id)).one[JsObject]
  } yield feature

  override def find(user: String, cat: Set[String], term: Option[String] = None): Future[Seq[JsObject]] = for {
    collection <- storage
    fs <- collection
      .find(obj(
        USER -> obj("$in" -> arr(user, DEF_USER_VAL)),
        CAT -> obj("$in" -> cat),
        "$text" -> obj("$search" -> term)))
      .projection(obj(DATA -> 0, USER -> 0))
      .cursor[JsObject]()
      .collect(-1, Cursor.FailOnError[Seq[JsObject]]())
  } yield fs
}
