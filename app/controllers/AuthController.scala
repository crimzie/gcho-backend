package controllers

import com.github.dwickern.macros.NameOf
import com.mohiva.play.silhouette.api.crypto.Base64AuthenticatorEncoder
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.AuthenticatorService
import com.mohiva.play.silhouette.api.util.{Credentials, PasswordHasher}
import com.mohiva.play.silhouette.api.{LoginInfo, Silhouette}
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticator
import com.mohiva.play.silhouette.impl.exceptions.{IdentityNotFoundException, InvalidPasswordException}
import com.mohiva.play.silhouette.impl.providers.{SocialProvider, _}
import daos.{UserDao, UserTokenDao}
import io.swagger.annotations._
import models.auth._
import org.joda.time.DateTime
import play.api.data.Form
import play.api.data.Forms._
import play.api.data.validation.Constraints._
import play.api.libs.json.{JsArray, Json}
import play.api.mvc._
import services.Mailer

import scala.concurrent.{ExecutionContext, Future}
import scala.language.postfixOps

case class AuthControllerConf(signupUrl: String, resetUrl: String, mailMock: Boolean)

case class SignUp(email: String, password: String, name: String)

case class SignIn(email: String, password: String, rememberMe: Boolean)

case class Email(email: String)

case class Password(password: String)

case class PasswordChange(oldPassword: String, newPassword: String)

object AuthController {
  val signUpForm        : Form[SignUp]         = Form apply mapping(
    NameOf.nameOf[SignUp](_.email) -> email,
    NameOf.nameOf[SignUp](_.password) -> nonEmptyText.verifying(minLength(6)),
    NameOf.nameOf[SignUp](_.name) -> nonEmptyText)(SignUp.apply)(SignUp.unapply)
  val signInForm        : Form[SignIn]         = Form apply mapping(
    NameOf.nameOf[SignIn](_.email) -> email,
    NameOf.nameOf[SignIn](_.password) -> nonEmptyText,
    NameOf.nameOf[SignIn](_.rememberMe) -> boolean)(SignIn.apply)(SignIn.unapply)
  val forgotPasswordForm: Form[String]         = Form apply single(
    NameOf.nameOf[Email](_.email) -> email)
  val newPasswordForm   : Form[String]         = Form apply single(
    NameOf.nameOf[Password](_.password) -> nonEmptyText.verifying(minLength(6)))
  val changePasswordForm: Form[PasswordChange] = Form apply mapping(
    NameOf.nameOf[PasswordChange](_.newPassword) -> nonEmptyText.verifying(minLength(6)),
    NameOf.nameOf[PasswordChange](_.oldPassword) -> nonEmptyText)(PasswordChange.apply)(PasswordChange.unapply)
}

@Api("Players")
class AuthController(
    components: ControllerComponents,
    silhouette: Silhouette[JWTEnv],
    socialProviderRegistry: SocialProviderRegistry,
    authInfoRepository: AuthInfoRepository,
    authenticatorEncoder: Base64AuthenticatorEncoder,
    credentialsProvider: CredentialsProvider,
    userDao: UserDao,
    tokenDao: UserTokenDao,
    passwordHasher: PasswordHasher,
    conf: AuthControllerConf,
    mailer: Mailer)(implicit ec: ExecutionContext) extends AbstractController(components) {
  scribe debug "Instantiating."

  import AuthController._

  lazy val authService: AuthenticatorService[JWTAuthenticator] = silhouette.env.authenticatorService
  val handleFormErrors: (Form[_]) => Future[Result] = f => Future successful BadRequest((JsArray.empty /: f.errors) {
    case (ja, e) => ja :+ Json.obj("key" -> e.key, "messages" -> Json.toJson(e.messages))
  })

  @ApiOperation(value = "New player signup form", code = 202)
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "body",
    value = "User credentials",
    required = true,
    dataType = "controllers.SignUp",
    paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid signup data"),
    new ApiResponse(code = 409, message = "Email already registered")))
  def signUp(): Action[AnyContent] = Action async { implicit request =>
    (signUpForm bindFromRequest) fold(
      success = data => userDao byMail data.email flatMap {
        case Some(u) if u.email.exists(_.confirmed) => Future successful Conflict
        case optUsr                                 =>
          val loginInfo = LoginInfo(CredentialsProvider.ID, data.email)
          val ev = CredentialsProvider.ID -> data.email
          val mail = Some(Mail(data.email, confirmed = false))
          val user = optUsr map (u => u.copy(email = mail, logins = u.logins + ev)) getOrElse
            User(name = data.name, email = mail, logins = Map(ev))
          for {
            _ <- userDao save user
            _ <- authInfoRepository save(loginInfo, passwordHasher hash data.password)
            token = UserToken(userId = user._id, email = data.email, signUp = true)
            _ <- tokenDao save token
            _ <- mailer welcome(user.name, data.email, conf.signupUrl + token._id)
          } yield if (conf.mailMock) Accepted(token._id) else Accepted
      },
      hasErrors = handleFormErrors)
  }

  @ApiOperation(value = "New player signup confirmation", code = 200, responseHeaders = Array(
    new io.swagger.annotations.ResponseHeader(
      name = "X-Auth-Token",
      response = classOf[String],
      description = "JWT session token")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 404, message = "Unknown token"),
    new ApiResponse(code = 410, message = "Token expired")))
  def signUpConfirm(tokenId: String): Action[AnyContent] = Action async { implicit request =>
    tokenDao find tokenId flatMap {
      case None                        => Future successful NotFound
      case Some(token) if token.signUp =>
        tokenDao remove token._id
        if (!token.isExpired) for {
          optUsr <- userDao byId token.userId
          authenticator <- authService create LoginInfo(CredentialsProvider.ID, token.email)
          value <- authService init authenticator
          _ <- userDao save optUsr.get.copy(email = Some(Mail(token.email, confirmed = true)))
          result <- authService embed(value, Ok)
        } yield result else Future successful Gone
    }
  }

  @ApiOperation(value = "Player login form", code = 200, responseHeaders = Array(
    new io.swagger.annotations.ResponseHeader(
      name = "X-Auth-Token",
      response = classOf[String],
      description = "JWT session token")))
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "body",
    value = "Login credentials",
    required = true,
    dataType = "controllers.SignIn",
    paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid signup data"),
    new ApiResponse(code = 401, message = "Password invalid"),
    new ApiResponse(code = 403, message = "Email not confirmed"),
    new ApiResponse(code = 404, message = "Player not found")))
  def signIn(): Action[AnyContent] = Action async { implicit request =>
    (signInForm bindFromRequest) fold(
      success = data => (for {
        loginInfo <- credentialsProvider authenticate Credentials(data.email, data.password)
        optUser <- userDao retrieve loginInfo
        authenticator <- authService create loginInfo
        value <- authService init
          (if (data.rememberMe) authenticator.copy(expirationDateTime = new DateTime plusMonths 1) else authenticator)
        result <- if (optUser.get.email.exists(_.confirmed))
          authService embed(value, Ok) else Future successful Forbidden
      } yield result) recover {
        case _: InvalidPasswordException  => Unauthorized
        case _: IdentityNotFoundException => NotFound
      },
      hasErrors = handleFormErrors)
  }

  @ApiOperation(value = "", code = 200, responseHeaders = Array(
    new io.swagger.annotations.ResponseHeader(
      name = "X-Auth-Token",
      response = classOf[String],
      description = "JWT session token")))
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "code",
    value = "Provider authentication code",
    required = true,
    dataType = "string",
    paramType = "query")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 303, message = "Authentication code not found"),
    new ApiResponse(code = 409, message = "This user already registered with another account for this provider"),
    new ApiResponse(code = 404, message = "Provider not found")))
  def social(
      @ApiParam(
        required = true,
        allowableValues = "google,facebook",
        value = "Authentication provider")
      providerId: String): Action[AnyContent] = Action async { implicit request =>
    (socialProviderRegistry get[SocialProvider] providerId fold Future.successful[Result](NotFound)) { provider =>
      for {
        either <- provider authenticate()
        result <- either.fold[Future[Result]](
          Future.successful,
          authInfo => for {
            profile <- provider retrieveProfile authInfo map (_.asInstanceOf[CommonSocialProfile])
            result <- userDao retrieve profile.loginInfo flatMap {
              case Some(_) => Future successful Ok
              case _       => profile.email map userDao.byMail getOrElse Future.successful(None) flatMap {
                case Some(u) if u.logins isDefinedAt profile.loginInfo.providerID => Future successful Conflict
                case optSameEml                                                   => for {
                  _ <- userDao save (optSameEml map { u =>
                    u.copy(logins = u.logins + (profile.loginInfo.providerID -> profile.loginInfo.providerKey))
                  } getOrElse User(
                    name = profile.fullName getOrElse "",
                    email = profile.email.map(m => Mail(m, confirmed = true)),
                    logins = Map(profile.loginInfo.providerID -> profile.loginInfo.providerKey)))
                  _ <- authInfoRepository save(profile.loginInfo, authInfo)
                } yield Ok
              }
            }
            result <- if (result == Ok) for {
              authenticator <- authService create profile.loginInfo
              token <- authService init authenticator
              result <- authService embed(token, result)
            } yield result else Future successful result
          } yield result)
      } yield result
    }
  }

  @ApiOperation(value = "Player logout", code = 204)
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "X-Auth-Token",
    value = "JWT session token",
    required = true,
    dataType = "string",
    paramType = "header")))
  @ApiResponses(value = Array(new ApiResponse(code = 401, message = "Invalid JWT token.")))
  def signOut(): Action[AnyContent] = silhouette.SecuredAction async { implicit request =>
    authService discard(request.authenticator, NoContent)
  }

  @ApiOperation(value = "Password reset form", code = 202)
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "body",
    value = "User email",
    required = true,
    dataType = "controllers.Email",
    paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid request data"),
    new ApiResponse(code = 404, message = "Player not found")))
  def resetPassword(): Action[AnyContent] = Action async { implicit request =>
    (forgotPasswordForm bindFromRequest) fold(
      success = email => userDao retrieve LoginInfo(CredentialsProvider.ID, email) flatMap {
        case None       => Future successful NotFound
        case Some(user) =>
          val token = UserToken(userId = user._id, email = email, signUp = false)
          for {
            _ <- tokenDao save token
            _ <- mailer resetPassword(email, conf.resetUrl + token._id)
          } yield if (conf.mailMock) Accepted(token._id) else Accepted
      },
      hasErrors = handleFormErrors)
  }

  @ApiOperation(value = "Password reset confirmation form", code = 200, responseHeaders = Array(
    new io.swagger.annotations.ResponseHeader(
      name = "X-Auth-Token",
      response = classOf[String],
      description = "JWT session token")))
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "body",
    value = "New password",
    required = true,
    dataType = "controllers.Password",
    paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 404, message = "Unknown token"),
    new ApiResponse(code = 410, message = "Token expired")))
  def resetPasswordConfirm(tokenId: String): Action[AnyContent] = Action async { implicit request =>
    (newPasswordForm bindFromRequest) fold(
      success = psw => tokenDao find tokenId flatMap {
        case Some(token) if !token.signUp =>
          tokenDao remove tokenId
          if (!token.isExpired) {
            val loginInfo = LoginInfo(CredentialsProvider.ID, token.email)
            for {
              _ <- authInfoRepository save(loginInfo, passwordHasher hash psw)
              authenticator <- authService create loginInfo
              value <- authService init authenticator
              result <- authService embed(value, Ok)
            } yield result
          } else Future successful Gone
        case _                            => Future successful NotFound
      },
      hasErrors = handleFormErrors)
  }

  @ApiOperation(value = "Change password form", code = 200)
  @ApiImplicitParams(Array(new ApiImplicitParam(
    name = "body",
    value = "User credentials",
    required = true,
    dataType = "controllers.PasswordChange",
    paramType = "body")))
  @ApiResponses(value = Array(
    new ApiResponse(code = 400, message = "Invalid signup data"),
    new ApiResponse(code = 409, message = "Player already registerd")))
  def changePassword(): Action[AnyContent] = silhouette.SecuredAction async { implicit request =>
    (changePasswordForm bindFromRequest) fold(
      success = {
        case PasswordChange(oldPsw, newPsw) => (for {
          li <- credentialsProvider authenticate Credentials(request.authenticator.loginInfo.providerKey, oldPsw)
          _ <- authInfoRepository save(li, passwordHasher hash newPsw)
        } yield Ok) recover {
          case _: InvalidPasswordException => Unauthorized
        }
      },
      hasErrors = handleFormErrors)
  }
}
