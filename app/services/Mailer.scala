package services

import com.google.inject.Inject
import play.api.i18n.Messages
import play.api.libs.mailer._
import play.api.{Configuration, Logger}

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

class Mailer @Inject()(configuration: Configuration, client: MailerClient)
                      (implicit ec: ExecutionContext, messages: Messages) {
  val from: String = configuration get[String] "mail.from"
  val replyTo: Seq[String] = Seq(configuration get[String] "mail.reply")

  def sendEmail(
                 recipient: String,
                 subject: String,
                 bodyHtml: Option[String],
                 bodyText: Option[String]): Future[Unit] = Future {
    client send Email(
      subject = subject,
      from = from,
      to = recipient :: Nil,
      bodyHtml = bodyHtml,
      bodyText = bodyText,
      replyTo = replyTo)
  } recover { case e => Logger error("Error sending email", e) } map (_ => ())

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
