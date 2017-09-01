package controllers

import java.util.NoSuchElementException

import com.google.inject.{Inject, Singleton}
import com.mohiva.play.silhouette.api.crypto.Base64AuthenticatorEncoder
import com.mohiva.play.silhouette.api.exceptions.ProviderException
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.api.util.{Credentials, PasswordHasher}
import com.mohiva.play.silhouette.api.{LoginInfo, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.{JWTAuthenticator, JWTAuthenticatorSettings}
import com.mohiva.play.silhouette.impl.providers.{SocialProvider, _}
import daos.{UserDao, UserTokenDao}
import models.auth._
import org.joda.time.DateTime
import play.api.Configuration
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.{JsArray, JsString}
import play.api.mvc._
import services.Mailer

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

object AuthController {

  case class SignUp(email: String, password: (String, String), name: String)

  case class SignIn(email: String, password: String, rememberMe: Boolean)

  val signUpForm: Form[SignUp] = Form apply mapping(
    "email" -> email,
    "password" -> (tuple("password1" -> (nonEmptyText verifying minLength(6)), "password2" -> nonEmptyText) verifying
      ("error.passwordsDontMatch", password => password._1 == password._2)),
    "name" -> nonEmptyText)(SignUp.apply)(SignUp.unapply)
  val signInForm: Form[SignIn] = Form apply mapping(
    "email" -> email,
    "password" -> nonEmptyText,
    "rememberMe" -> boolean)(SignIn.apply)(SignIn.unapply)
  val forgotPasswordForm: Form[String] = Form apply single("email" -> email)
  val newPasswordForm = Form(tuple(
    "password1" -> (nonEmptyText verifying minLength(6)),
    "password2" -> nonEmptyText) verifying("error.passwordsDontMatch", password => password._1 == password._2))
  val changePasswordForm = Form(tuple(
    "oldPassword" -> nonEmptyText,
    "newPassword" -> (nonEmptyText verifying minLength(6))))
}

@Singleton
class AuthController @Inject()(
                                components: ControllerComponents,
                                silhouette: Silhouette[JWTEnv],
                                socialProviderRegistry: SocialProviderRegistry,
                                authInfoRepository: AuthInfoRepository,
                                authenticatorEncoder: Base64AuthenticatorEncoder,
                                credentialsProvider: CredentialsProvider,
                                userDao: UserDao,
                                tokenDao: UserTokenDao,
                                passwordHasher: PasswordHasher,
                                assets: Assets,
                                configuration: Configuration,
                                mailer: Mailer)(implicit ec: ExecutionContext) extends InjectedController {
  setControllerComponents(components)

  import AuthController._

  lazy val authService: AuthenticatorService[JWTAuthenticator] = silhouette.env.authenticatorService
  lazy val jWTSettings: JWTAuthenticatorSettings =
    JWTAuthenticatorSettings("token", sharedSecret = configuration get[String] "silhouette.authenticator.sharedSecret")
  private val handleFormErrors = (form: Form[_]) => Future successful BadRequest((JsArray.empty /: form.errors) {
    case (ja, e) => ja :+ JsString(e.message)
  })

  def signUp(): Action[AnyContent] = Action async { implicit request =>
    (signUpForm bindFromRequest) fold(
      success = data => userDao byMail data.email flatMap {
        case Some(u) if u.email.exists(_.confirmed) => Future successful Conflict
        case optUsr =>
          val loginInfo = LoginInfo(CredentialsProvider.ID, data.email)
          val ev = CredentialsProvider.ID -> data.email
          val mail = Some(Mail(data.email, confirmed = false))
          val user = optUsr map (u => u.copy(email = mail, logins = u.logins + ev)) getOrElse
            User(name = data.name, email = mail, logins = Map(ev))
          for {
            _ <- userDao save user
            _ <- authInfoRepository add(loginInfo, passwordHasher hash data.password._1)
            token = UserToken(userId = user._id, email = data.email, signUp = true)
            _ <- tokenDao save token
            _ <- mailer welcome(user.name, data.email, s"https://${request.host}/auth/signup/confirm/${token._id}")
          } yield Accepted
      },
      hasErrors = handleFormErrors)
  }

  def signUpConfirm(tokenId: String): Action[AnyContent] = Action async { implicit request =>
    tokenDao find tokenId flatMap {
      case Some(token) if token.signUp =>
        tokenDao remove token._id
        if (!token.isExpired) for {
          optUsr <- userDao byId token.userId
          authenticator <- authService create LoginInfo(CredentialsProvider.ID, token.email)
          value <- authService init authenticator
          _ <- userDao save optUsr.get.copy(email = Some(Mail(token.email, confirmed = true)))
          result <- authService embed(value, Ok)
        } yield result else Future successful Gone
      case _ => Future successful BadRequest
    }
  }

  def signIn(): Action[AnyContent] = Action async { implicit request =>
    (signInForm bindFromRequest) fold(
      success = data => (for {
        loginInfo <- credentialsProvider authenticate Credentials(data.email, data.password)
        optUser <- userDao retrieve loginInfo
        authenticator <- authService create loginInfo
        value <- authService init
          (if (data.rememberMe) authenticator.copy(expirationDateTime = new DateTime plusMonths 1) else authenticator)
        result <- if (optUser.get.email.exists(_.confirmed)) authService embed(value, Accepted)
        else Future successful Unauthorized
      } yield result) recover {
        case _: NoSuchElementException => Unauthorized
        case _: ProviderException => Unauthorized
      },
      hasErrors = handleFormErrors)
  }

  def social(providerId: String): Action[AnyContent] = Action async { implicit request =>
    (socialProviderRegistry.get[SocialProvider](providerId) fold Future.successful[Result](NotFound)) { provider =>
      for {
        either <- provider authenticate()
        result <- either.fold[Future[Result]](
          fa = Future.successful,
          fb = authInfo => for {
            profile <- provider retrieveProfile authInfo map (_.asInstanceOf[CommonSocialProfile])
            result <- userDao retrieve profile.loginInfo flatMap {
              case Some(_) => Future successful Ok
              case _ => for {
                result <- profile.email map userDao.byMail getOrElse Future.successful(None) flatMap {
                  case Some(u) if u.logins isDefinedAt profile.loginInfo.providerID => Future successful Conflict
                  case optSameEml => for {
                    _ <- userDao save (optSameEml map { u =>
                      u.copy(logins = u.logins + (profile.loginInfo.providerID -> profile.loginInfo.providerKey))
                    } getOrElse User(
                      name = profile.fullName getOrElse "",
                      email = profile.email.map(m => Mail(m, confirmed = true)),
                      logins = Map(profile.loginInfo.providerID -> profile.loginInfo.providerKey)))
                    _ <- authInfoRepository save(profile.loginInfo, authInfo)
                  } yield Ok
                }
              } yield result
            }
            authenticator <- authService create profile.loginInfo
            token <- authService init authenticator
          } yield if (result == Ok) result.withCookies(
            Cookie("authToken", token, Some(60), "/", Some(request.host), secure = true, httpOnly = false))
          else result)
      } yield result
    }
  }

  def signOut(): Action[AnyContent] = silhouette.SecuredAction async { implicit request =>
    authService discard(request.authenticator, Results.Ok)
  }

  def resetPassword(): Action[AnyContent] = Action async { implicit request =>
    (forgotPasswordForm bindFromRequest) fold(
      success = email => userDao retrieve LoginInfo(CredentialsProvider.ID, email) flatMap {
        case Some(user) =>
          val token = UserToken(userId = user._id, email = email, signUp = false)
          for {
            _ <- tokenDao save token
            _ <- mailer resetPassword(email, s"https://${request.host}/auth/reset/confirm/${token._id}")
          } yield Accepted
        case _ => Future successful NotFound
      },
      hasErrors = handleFormErrors)
  }

  def resetPasswordConfirm(tokenId: String): Action[AnyContent] = Action async { implicit request =>
    (newPasswordForm bindFromRequest) fold(
      success = passwords => tokenDao find tokenId flatMap {
        case Some(token) if !token.signUp =>
          tokenDao remove tokenId
          if (!token.isExpired) {
            val loginInfo = LoginInfo(CredentialsProvider.ID, token.email)
            for {
              _ <- authInfoRepository save(loginInfo, passwordHasher hash passwords._1)
              authenticator <- authService create loginInfo
              value <- authService init authenticator
              result <- authService embed(value, Accepted)
            } yield result
          } else Future successful Gone
        case _ => Future successful NotFound
      },
      hasErrors = handleFormErrors)
  }

  def changePassword(): Action[AnyContent] = silhouette.SecuredAction async { implicit request =>
    (changePasswordForm bindFromRequest) fold(
      success = {
        case (oldPasw, newPasw) => for {
          li <- credentialsProvider authenticate Credentials(CredentialsProvider.ID, oldPasw)
          _ <- authInfoRepository save(li, passwordHasher hash newPasw)
        } yield Ok
      },
      hasErrors = handleFormErrors)
  }
}
