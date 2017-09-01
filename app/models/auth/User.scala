package models
package auth

import com.mohiva.play.silhouette.api.Identity

import scala.language.postfixOps
import scala.util.Random

case class User(
                 _id: String = Random.alphanumeric take 8 mkString,
                 name: String,
                 logins: Map[String, String],
                 email: Option[Mail]) extends Identity

case class Mail(address: String, confirmed: Boolean)

case class Auth(id: String, key: String)
