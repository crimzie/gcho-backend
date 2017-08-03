import models.charlist._
import org.mongodb.scala.{Document, MongoClient}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import play.api.libs.json.Json

import scala.concurrent.Await
import scala.concurrent.duration._

object Preloader extends App {
  private def load[A](client: MongoClient, collection: String, parser: Parser[A]) = {
    println(s"""Clearing db "$db" collection "$collection"...""")
    Await ready(client getDatabase db getCollection collection drop() toFuture(), 10.seconds) onSuccess {
      case Seq(_) => println("Cleared.")
    }
    println(s"Loading basic $collection...")
    val seq = parser.seq map { x => Document(Json.toJson(x)(parser.tjs).toString) }
    Await ready(client getDatabase db getCollection collection insertMany seq toFuture(), 30.seconds) onSuccess {
      case Seq(_) => println(s"""Basic $collection loaded to mongo db "$db" collection "$collection".""")
    }
  }

  import daos.MongoCollections._

  println("Opening connection...")
  private val db = "gurps"
  private val client = MongoClient()
  load[FlaggedTrait](client, TRAITS, new TraitsParser("/adv.xml"))
  load[FlaggedSkill](client, SKILLS, new SkillsParser("/skl.xml"))
  load[FlaggedTechnique](client, TECHNIQUES, new TechniquesParser("/skl.xml"))
  load[FlaggedArmor](client, ARMORS, new ArmorParser("/eqp.xml"))
  load[FlaggedWeapon](client, WEAPONS, new WeaponsParser("/eqp.xml"))
  load[FlaggedItem](client, ITEMS, new ItemsParser("/eqp.xml"))
  client close()
}