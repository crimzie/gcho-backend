import com.mohiva.play.silhouette.crypto.JcaSignerSettings
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticatorSettings
import com.mohiva.play.silhouette.impl.providers.OAuth2Settings
import com.mohiva.play.silhouette.impl.providers.state.CsrfStateSettings
import controllers.{AssetsConfiguration, AuthControllerConf}
import net.sf.ehcache.config.CacheConfiguration
import play.api.http.HttpErrorConfig
import play.api.libs.mailer.SMTPConfiguration
import services.MailerConf

import scala.concurrent.duration._
import scala.language.postfixOps
import scala.util.Random

object Conf {
  private val hostname: String = Option(System getenv "GCHO_HOSTNAME") getOrElse "0.0.0.0:9000"
  private val redirectPath: String = Option(System getenv "GCHO_REDIRPATH") getOrElse "/auth"
  private val confirmPath: String = Option(System getenv "GCHO_EMAILPATH") getOrElse "/auth"
  private val baseRedirectUrl: String = s"https://$hostname$redirectPath"
  private val baseMailUrl = s"https://$hostname$confirmPath"

  val devConf = Conf(
    hostName = hostname,
    mongoHostPort = Option(System getenv "MONGO") getOrElse "127.0.0.1:27017",
    dbName = "gurps",
    jwtAuthConf = JWTAuthenticatorSettings(
      issuerClaim = "GCHO",
      authenticatorExpiry = 12 hours,
      authenticatorIdleTimeout = Some(14 days),
      sharedSecret = Option(System getenv "GCHO_AUTH_SECRET") getOrElse "changeme"),
    jcaSignerSettings = JcaSignerSettings(key = Option(System getenv "GCHO_SIGNER_KEY") getOrElse "changeme"),
    csrfStateSettings = CsrfStateSettings(),
    facebookConf = OAuth2Settings(
      authorizationURL = Some("https://graph.facebook.com/v2.8/oauth/authorize"),
      accessTokenURL = "https://graph.facebook.com/v2.8/oauth/access_token",
      redirectURL = Some(s"$baseRedirectUrl/facebook"),
      clientID = Option(System getenv "GCHO_FBOOK_ID") getOrElse "338991583219498",
      clientSecret = Option(System getenv "GCHO_FBOOK_SECRET") getOrElse "5c1dbc62a299967dcbab37820069f7db",
      scope = Some("public_profile email")),
    googleConf = OAuth2Settings(
      authorizationURL = Some("https://accounts.google.com/o/oauth2/auth"),
      accessTokenURL = "https://accounts.google.com/o/oauth2/token",
      redirectURL = Some(s"$baseRedirectUrl/google"),
      clientID = Option(System getenv "GCHO_GOOG_ID") getOrElse
        "405952316967-nogcnup6lg1tnimaj48pihdabrlvh4su.apps.googleusercontent.com",
      clientSecret = Option(System getenv "GCHO_GOOG_SECRET") getOrElse "-235qHSDCT3qRFi6gUAFrKV0",
      scope = Some("profile email")),
    mailerConf = MailerConf(
      from = Option(System getenv "GCHO_MAILFROM") getOrElse "GCHO <mailrobot@gcho.com>",
      replyTo = Option(System getenv "GCHO_MAILREPLY").getOrElse("No reply <noreply@gcho.com>") :: Nil),
    smtpConf = SMTPConfiguration(
      host = "0.0.0.0",
      port = 0,
      mock = true),
    authControllerConf = AuthControllerConf(
      signupUrl = s"$baseMailUrl/signup",
      resetUrl = s"$baseMailUrl/reset",
      mailMock = true),
    cacheConf = new CacheConfiguration("Cache", 500),
    httpErrorConf = HttpErrorConfig(showDevErrors = true),
    assetsConf = AssetsConfiguration())

  val testConf: Conf = devConf

  lazy val prodConf: Conf = devConf.copy(
    jwtAuthConf = devConf.jwtAuthConf.copy(
      sharedSecret = Option(System getenv "GCHO_AUTH_SECRET") getOrElse Random.alphanumeric.take(16).mkString),
    jcaSignerSettings = devConf.jcaSignerSettings.copy(
      key = Option(System getenv "GCHO_AUTH_SECRET") getOrElse Random.alphanumeric.take(16).mkString,
      pepper = Option(System getenv "GCHO_SIGNER_PEPPER") getOrElse devConf.jcaSignerSettings.pepper),
    smtpConf = devConf.smtpConf.copy(
      mock = false,
      host = Option(System getenv "GCHO_SMTP_HOST") getOrElse "127.0.0.1",
      port = Option(System getenv "GCHO_SMTP_PORT") getOrElse "25" toInt,
      user = Option(System getenv "GCHO_SMTP_LOGIN"),
      password = Option(System getenv "GCHO_SMTP_PASSWORD"),
      ssl = true,
      tls = true))
}

case class Conf(
                 hostName: String,
                 mongoHostPort: String,
                 dbName: String,
                 jwtAuthConf: JWTAuthenticatorSettings,
                 jcaSignerSettings: JcaSignerSettings,
                 csrfStateSettings: CsrfStateSettings,
                 facebookConf: OAuth2Settings,
                 googleConf: OAuth2Settings,
                 mailerConf: MailerConf,
                 smtpConf: SMTPConfiguration,
                 authControllerConf: AuthControllerConf,
                 cacheConf: CacheConfiguration,
                 httpErrorConf: HttpErrorConfig,
                 assetsConf: AssetsConfiguration)
