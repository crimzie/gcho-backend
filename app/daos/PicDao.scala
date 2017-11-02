package daos

import reactivemongo.api.DefaultDB
import reactivemongo.bson._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

trait PicDao {
  def save(id: String, pic: Array[Byte]): Future[Unit]

  def load(id: String): Future[Option[Array[Byte]]]

  def delete(id: String): Future[Unit]
}

class MongoPicDao(override val mongo: Future[DefaultDB])(implicit val ec: ExecutionContext)
  extends PicDao with BSONMongoDao {
  scribe debug "Instantiating."
  override val colName: String = "pics"
  val PIC = "pic"
  val ID  = "_id"

  override def save(id: String, pic: Array[Byte]): Future[Unit] = for {
    c <- storage
    _ <- c update(d :~ ID -> id, d :~ ID -> id :~ PIC -> pic, upsert = true)
  } yield ()

  override def load(id: String): Future[Option[Array[Byte]]] = for {
    c <- storage
    optD <- c.find(d :~ ID -> id).one[BSONDocument]
  } yield for {
    d <- optD
    a <- d.getAs[Array[Byte]](PIC)
  } yield a

  override def delete(id: String): Future[Unit] = for {
    c <- storage
    _ <- c remove d :~ ID -> id
  } yield ()
}
