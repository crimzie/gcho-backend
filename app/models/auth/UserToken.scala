package models
package auth

import org.joda.time.DateTime

import scala.language.postfixOps
import scala.util.Random

case class UserToken(
                      _id: String = Random.alphanumeric take 12 mkString,
                      userId: String,
                      email: String,
                      expire: Long = new DateTime plusHours 12 getMillis,
                      signUp: Boolean) {
  lazy val isExpired: Boolean = new DateTime(expire).isBeforeNow
}
