package models
package auth

import com.github.dwickern.macros.NameOf
import com.mohiva.play.silhouette.api.util.PasswordInfo
import com.mohiva.play.silhouette.api.{AuthInfo, Identity, LoginInfo}
import com.mohiva.play.silhouette.impl.providers.oauth2.{FacebookProvider, GoogleProvider}
import com.mohiva.play.silhouette.impl.providers.{CredentialsProvider, OAuth2Info}
import reactivemongo.bson.{BSONArray, BSONDocument, BSONDocumentHandler, BSONHandler, Macros}

import scala.language.postfixOps
import scala.util.Random

case class User(
                 _id: String = Random.alphanumeric take 6 mkString,
                 name: String,
                 auths: Map[String, Auth],
                 email: Option[Mail]) extends Identity

object User {
  implicit val loginBsonHandler: BSONDocumentHandler[LoginInfo] = Macros.handler[LoginInfo]
  implicit val oAuth2BsonHandler: BSONDocumentHandler[OAuth2Info] = Macros.handler[OAuth2Info]
  implicit val passwordBsonHandler: BSONDocumentHandler[PasswordInfo] = Macros.handler[PasswordInfo]
  private val handlers = Map[String, BSONDocumentHandler[_]](
    FacebookProvider.ID -> oAuth2BsonHandler,
    GoogleProvider.ID -> oAuth2BsonHandler,
    CredentialsProvider.ID -> passwordBsonHandler)
  implicit val authMapBsonHandler: BSONHandler[BSONArray, Map[String, Auth]] =
    BSONHandler[BSONArray, Map[String, Auth]](
      read = arr => arr.values.map { b =>
        val bd = b.asInstanceOf[BSONDocument]
        val li: LoginInfo = bd.getAs[LoginInfo](Auth.LINF).get
        val ai: AuthInfo =
          handlers(li.providerID).read(bd.get(Auth.AINF).get.asInstanceOf[BSONDocument]).asInstanceOf[AuthInfo]
        li.providerID -> Auth(li, ai)
      }(collection.breakOut),
      write = map => BSONArray(for {(_, c) <- map} yield {
        val bson = c.auth match {
          case oa: OAuth2Info => oAuth2BsonHandler write oa
          case pw: PasswordInfo => passwordBsonHandler write pw
        }
        BSONDocument(Auth.LINF -> c.login, Auth.AINF -> bson)
      }))
  implicit val mailHandler: BSONDocumentHandler[Mail] = Macros.handler[Mail]
  implicit val userHandler: BSONDocumentHandler[User] = Macros.handler[User]
}

case class Mail(address: String, confirmed: Boolean)

case class Auth(login: LoginInfo, auth: AuthInfo)

object Auth {
  val LINF: String = NameOf.nameOf[Auth](_.login)
  val AINF: String = NameOf.nameOf[Auth](_.auth)
}
