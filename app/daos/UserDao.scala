package daos

import com.github.dwickern.macros.NameOf
import com.google.inject.{ImplementedBy, Inject, Singleton}
import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService
import models.auth.{Auth, Mail, User}
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.Ascending
import reactivemongo.bson.{BSONArray, BSONDocument, BSONDocumentHandler, BSONHandler, Macros}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[MongoUserDao])
trait UserDao extends IdentityService[User] {
  def save(user: User): Future[Unit]

  def byId(id: String): Future[Option[User]]

  def byMail(mail: String): Future[Option[User]]
}

@Singleton
class MongoUserDao @Inject()(override val mongo: ReactiveMongoApi)(implicit val ec: ExecutionContext)
  extends UserDao with BSONMongoDao {
  override val colName = "players"
  implicit val authBsonHandler: BSONDocumentHandler[Auth] = Macros.handler[Auth]
  implicit val loginsMapBsonHandler: BSONHandler[BSONArray, Map[String, String]] =
    BSONHandler[BSONArray, Map[String, String]](
      read = arr => arr.values.map { jv =>
        val auth = jv.asInstanceOf[BSONDocument].as[Auth]
        auth.id -> auth.key
      }(collection.breakOut),
      write = map => BSONArray(map.map { case (id, key) => authBsonHandler write Auth(id, key) }))
  implicit val mailHandler: BSONDocumentHandler[Mail] = Macros.handler[Mail]
  implicit val userHandler: BSONDocumentHandler[User] = Macros.handler[User]
  val ID: String = NameOf.nameOf[User](_._id)
  val LGNS: String = NameOf.nameOf[User](_.logins)
  val PID: String = NameOf.nameOf[Auth](_.id)
  val KEY: String = NameOf.nameOf[Auth](_.key)
  val EML: String = NameOf.nameOf[User](_.email)
  val ADRS: String = NameOf.nameOf[Mail](_.address)

  storage map (_.indexesManager ensure
    Index(s"$EML.$ADRS" -> Ascending :: Nil, Some(s"bin-$EML.$ADRS-1"), unique = true))
  storage map (_.indexesManager ensure Index(
    s"$LGNS.$PID" -> Ascending :: s"$LGNS.$KEY" -> Ascending :: Nil,
    Some(s"bin-$LGNS.$PID-$LGNS.$KEY-1"),
    unique = true))

  override def save(user: User): Future[Unit] = for {
    c <- storage
    _ <- c update(d :~ ID -> user._id, user, upsert = true)
  } yield ()

  override def byId(id: String): Future[Option[User]] = for {
    c <- storage
    u <- c.find(d :~ ID -> id).one[User]
  } yield u

  override def byMail(mail: String): Future[Option[User]] = for {
    c <- storage
    u <- c.find(d :~ s"$EML.$ADRS" -> mail).one[User]
  } yield u

  override def retrieve(loginInfo: LoginInfo): Future[Option[User]] = for {
    c <- storage
    sel = d :~ LGNS -> (d :~ "$elemMatch" -> (d :~ PID -> loginInfo.providerID :~ KEY -> loginInfo.providerKey))
    u <- c.find(sel).one[User]
  } yield u
}
