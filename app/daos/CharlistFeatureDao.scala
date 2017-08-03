package daos

import akka.actor.FSM.->
import com.google.inject.{ImplementedBy, Inject, Singleton}
//import models.charlist.Charlist._
import models.charlist._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.{JsObject, Json}
import play.modules.reactivemongo.ReactiveMongoApi
//import play.modules.reactivemongo.json._
import reactivemongo.play.json.collection.JSONCollection

import scala.concurrent.Future

@ImplementedBy(classOf[MongoCharlistFeatureDao])
trait CharlistFeatureDao {
  def save[F <: FlaggedFeature](feature: F): Future[Unit]

  def find[F <: FlaggedFeature](id: String): Future[F]

  def find[F <: FlaggedFeature](cat: Seq[String], term: String = ""): Future[Seq[F]]
}

@Singleton
class MongoCharlistFeatureDao @Inject()(mongo: ReactiveMongoApi) extends CharlistFeatureDao {
  val storage: Future[JSONCollection] = mongo.database map (_ collection[JSONCollection] "features")
  private val strMap = Map(
    TRAITS -> "name",
    SKILLS -> "skillString",
    TECHNIQUES -> "tchString",
    WEAPONS -> "name",
    ARMORS -> "name",
    ITEMS -> "name")

  def save[F <: FlaggedFeature](feature: F): Future[Unit] = for {
    collection <- storage
    _ <- collection insert feature
  } yield ()

  def find[F <: FlaggedFeature](id: String): Future[Option[F]] = for {
    collection <- storage
    feature <- collection.find(Json obj "_id" -> id).one[F]
  } yield feature

  private val docIdToJson: (String, ) => JsObject = (col, doc) =>
    (Json obj "id" -> (doc get "_id").get.asObjectId.getValue.toString) ++
      (Json obj "name" -> (doc get "data").get.asDocument.get(strMap apply col).asString.getValue)

  def find[F <: FlaggedFeature](cat: Seq[String], term: String = ""): Future[Seq[F]] = for {
    collection <- storage
    collection.find(Json obj "data.category" -> (Json obj "$in" -> cat)).projection()
  }

  {
    val c = collMap(col)
    (if (cat != Nil) c find.in("data.category", cat: _*)
    else c find()
    ) withFilter {
      _.get("data").get.asDocument.get(strMap(col)).asString.getValue.toLowerCase.contains(term.toLowerCase)
    } map (docIdToJson(col, _)) toFuture()
  } // TODO: regex search
}
