package daos

import com.google.inject.{Inject, Singleton}
import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService
import models.auth.User
import models.auth.User._
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.collections.bson._
import reactivemongo.bson._

import scala.concurrent.Future

trait UserDao extends IdentityService[User] {
  def save(user: User): Future[Unit]

  def byId(id: String): Future[Option[User]]
}

@Singleton
class MongoUserDao @Inject()(mongo: ReactiveMongoApi) extends UserDao {
  private val storage: Future[BSONCollection] = mongo.database map (_ collection[BSONCollection] "characters")
  private val d = BSONDocument.empty

  override def save(user: User): Future[Unit] = for {
    c <- storage
    _ <- c update(d :~ "_id" -> user._id, user, upsert = true)
  } yield ()

  override def byId(id: String): Future[Option[User]] = for {
    c <- storage
    u <- c.find(d :~ "_id" -> id).one[User]
  } yield u

  override def retrieve(loginInfo: LoginInfo): Future[Option[User]] = for {
    c <- storage
    u <- c.find(d :~ "logins" -> (d :~ "$elemMatch" -> (d :~ "l" -> loginInfo))).one[User]
  } yield u
}
