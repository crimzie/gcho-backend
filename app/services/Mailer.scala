package services

import java.util.Locale

import play.api.i18n.{Lang, Messages, MessagesApi}
import play.api.libs.mailer._

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

case class MailerConf(from: String, replyTo: Seq[String])

class Mailer(conf: MailerConf, val client: MailerClient, messagesApi: MessagesApi)(implicit ec: ExecutionContext) {
  scribe debug "Instantiating."
  lazy implicit val messages: Messages = messagesApi preferred Lang(Locale.ENGLISH) :: Nil

  def sendEmail(
      recipient: String,
      subject: String,
      bodyHtml: Option[String],
      bodyText: Option[String]): Future[Unit] = Future {
    client send Email(
      subject = subject,
      from = conf.from,
      to = recipient :: Nil,
      bodyHtml = bodyHtml,
      bodyText = bodyText,
      replyTo = conf.replyTo)
  } recover { case e => scribe error("Error sending email", e) } map (_ => ())

  def welcome(name: String, email: String, link: String): Future[Unit] = sendEmail(
    email,
    "Welcome",
    Some(views.html.email.welcome(name, link) toString),
    Some(views.html.email.welcomeText(name, link) toString))

  def resetPassword(email: String, link: String): Future[Unit] = sendEmail(
    email,
    "Password reset",
    Some(views.html.email.resetPassword(email, link) toString),
    Some(views.html.email.resetPasswordText(email, link) toString))
}
