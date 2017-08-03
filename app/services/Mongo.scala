package services

import com.google.inject.{Inject, Singleton}
import org.mongodb.scala.{MongoClient, MongoDatabase}
import play.Logger
import play.api.Configuration
import play.api.inject.ApplicationLifecycle

import scala.concurrent.Future

@Singleton
class Mongo @Inject()(applicationLifecycle: ApplicationLifecycle, configuration: Configuration) {
  val client: MongoClient = MongoClient()
  val db: MongoDatabase = client getDatabase configuration.getString("mongo.db.name").get
  applicationLifecycle addStopHook { () =>
    Logger warn "Closing Mongo connection"
    Future successful client.close()
  }
}