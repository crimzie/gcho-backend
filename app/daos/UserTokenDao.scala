package daos

import com.github.dwickern.macros.NameOf
import models.auth.UserToken
import reactivemongo.api.DefaultDB
import reactivemongo.bson.{BSONDocumentHandler, Macros}

import scala.concurrent.{ExecutionContext, Future}

trait UserTokenDao {
  def find(id: String): Future[Option[UserToken]]

  def save(token: UserToken): Future[Unit]

  def remove(id: String): Future[Unit]
}

class MongoUserTokenDao(override val mongo: Future[DefaultDB])(implicit val ec: ExecutionContext)
  extends UserTokenDao with BSONMongoDao {
  scribe debug "Instantiating."
  override val colName                                          = "usertokens"
  implicit val userTokenHandler: BSONDocumentHandler[UserToken] = Macros.handler[UserToken]
  val ID: String = NameOf.nameOf[UserToken](_._id)

  def find(id: String): Future[Option[UserToken]] = for {
    c <- storage
    t <- c.find(d :~ ID -> id).one[UserToken]
  } yield t

  def save(token: UserToken): Future[Unit] = for {
    c <- storage
    _ <- c insert token
  } yield ()

  def remove(id: String): Future[Unit] = for {
    c <- storage
    _ <- c remove d :~ ID -> id
  } yield ()
}
