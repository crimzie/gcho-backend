package daos

import com.google.inject.{ImplementedBy, Inject, Singleton}
import com.mohiva.play.silhouette.api.LoginInfo
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import play.modules.reactivemongo.ReactiveMongoApi
import reactivemongo.bson.{BSONDocument, BSONDocumentHandler, Macros}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[MongoPasswordInfoDao])
trait PasswordInfoDao extends DelegableAuthInfoDAO[PasswordInfo]

@Singleton
class MongoPasswordInfoDao @Inject()(override val mongo: ReactiveMongoApi)(override implicit val ec: ExecutionContext)
  extends PasswordInfoDao with BSONMongoDao {
  override val colName: String = "passwords"
  implicit val loginInfoHandler: BSONDocumentHandler[LoginInfo] = Macros.handler[LoginInfo]
  implicit val passwordInfoHandler: BSONDocumentHandler[PasswordInfo] = Macros.handler[PasswordInfo]
  val LGN = "login"
  val PSW = "paswd"

  override def find(loginInfo: LoginInfo): Future[Option[PasswordInfo]] = for {
    c <- storage
    b <- c.find(d :~ LGN -> loginInfo).one[BSONDocument]
  } yield b.flatMap(_.getAs[PasswordInfo](PSW))

  override def add(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] = for {
    c <- storage
    _ <- c insert d :~ LGN -> loginInfo :~ PSW -> authInfo
  } yield authInfo

  override def update(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] = for {
    c <- storage
    _ <- c.update(d :~ LGN -> loginInfo, d :~ "$set" -> (d :~ PSW -> authInfo))
  } yield authInfo

  override def save(loginInfo: LoginInfo, authInfo: PasswordInfo): Future[PasswordInfo] = for {
    c <- storage
    _ <- c.update(d :~ LGN -> loginInfo, d :~ "$set" -> (d :~ PSW -> authInfo), upsert = true)
  } yield authInfo

  override def remove(loginInfo: LoginInfo): Future[Unit] = for {
    c <- storage
    _ <- c remove d :~ LGN -> loginInfo
  } yield ()
}
