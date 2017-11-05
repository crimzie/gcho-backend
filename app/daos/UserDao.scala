package daos

import com.github.dwickern.macros.NameOf
import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.services.IdentityService
import models.auth.{Auth, Mail, User}
import reactivemongo.api.DefaultDB
import reactivemongo.api.indexes.Index
import reactivemongo.api.indexes.IndexType.Ascending
import reactivemongo.bson._

import scala.concurrent.{ExecutionContext, Future}

trait UserDao extends IdentityService[User] {
  def save(user: User): Future[Unit]

  def byId(id: String): Future[Option[User]]

  def byMail(mail: String): Future[Option[User]]
}

class MongoUserDao(override val mongo: Future[DefaultDB])(implicit val ec: ExecutionContext)
  extends UserDao with BSONMongoDao {
  scribe debug "Instantiating."
  override val colName = "players"

  val ID  : String = NameOf.nameOf[User](_._id)
  val NAME: String = NameOf.nameOf[User](_.name)
  val LGNS: String = NameOf.nameOf[User](_.logins)
  val PID : String = NameOf.nameOf[Auth](_.id)
  val KEY : String = NameOf.nameOf[Auth](_.key)
  val EML : String = NameOf.nameOf[User](_.email)
  val ADRS: String = NameOf.nameOf[Mail](_.address)

  implicit val mapHandler : BSONDocumentHandler[Map[String, String]] = BSONDocumentHandler[Map[String, String]](
    read = _.elements.map { e => e.name -> e.value.asInstanceOf[BSONString].value }(collection.breakOut),
    write = m => BSONDocument apply m.map { t => BSONElement(t._1, BSONString(t._2)) })
  implicit val mailHandler: BSONDocumentHandler[Mail]                = Macros.handler[Mail]
  implicit val userHandler: BSONDocumentHandler[User]                = Macros.handler[User]

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
    sel = d :~ s"$LGNS.${loginInfo.providerID}" -> loginInfo.providerKey
    u <- c.find(sel).one[User]
  } yield u
}
