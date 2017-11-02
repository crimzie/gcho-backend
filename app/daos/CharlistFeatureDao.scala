package daos

import com.github.dwickern.macros.NameOf
import models.charlist.FeatureEntry._
import models.charlist._
import play.api.libs.json.JsObject
import play.api.libs.json.Json.{arr, obj}
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.{Ascending, Text}
import reactivemongo.api.{Cursor, DefaultDB}
import reactivemongo.play.json._
import reactivemongo.play.json.collection.JSONCollection
import services.defaults._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps
import scala.util.{Failure, Success}

trait CharlistFeatureDao {
  def save(feature: FeatureEntry[Feature]): Future[Unit]

  def remove(user: String, id: String): Future[Unit]

  def find(user: String, id: String): Future[Option[JsObject]]

  def find(user: String, cat: Set[String], term: Option[String] = None): Future[Seq[JsObject]]
}

class MongoCharlistFeatureDao(mongo: Future[DefaultDB])(implicit ec: ExecutionContext) extends CharlistFeatureDao {
  scribe debug "Instantiating."
  val storage: Future[JSONCollection] = mongo map (_ collection[JSONCollection] "features")

  private val ID           = NameOf.nameOf[FeatureEntry[_]](_._id)
  private val USER         = NameOf.nameOf[FeatureEntry[_]](_.user)
  private val CAT          = NameOf.nameOf[FeatureEntry[_]](_.cat)
  private val NAME         = NameOf.nameOf[Feature](_.str)
  private val DATA         = NameOf.nameOf[FeatureEntry[_]](_.data)
  private val DEF_USER_VAL = ""

  storage map (_.indexesManager ensure Index(USER -> Ascending :: Nil, name = Some(s"bin-$USER-1")))
  storage map (_.indexesManager ensure Index(CAT -> Ascending :: Nil, name = Some(s"bin-$CAT-1")))
  storage map (_.indexesManager ensure Index(NAME -> Text :: Nil, name = Some(s"txt-$NAME")))

  private val defaultsPreloading: Future[Unit] = for {
    s <- storage
    _ <- s remove obj(USER -> DEF_USER_VAL)
    stream = DefaultTraits.parse("/adv.xml") ++ DefaultSkills.parse("/skl.xml") ++
      DefaultTechniques.parse("/skl.xml") ++ DefaultArmor.parse("/eqp.xml") ++
      DefaultWeapons.parse("/eqp.xml") ++ DefaultItems.parse("/eqp.xml")
    _ <- s bulkInsert(stream, ordered = false)
  } yield ()
  defaultsPreloading onComplete {
    case Success(_) => scribe info "Default features collection loaded to db."
    case Failure(t) =>
      scribe warn "Failed to load default features collection to db:"
      scribe debug t
  }

  override def save(feature: FeatureEntry[Feature]): Future[Unit] = for {
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
