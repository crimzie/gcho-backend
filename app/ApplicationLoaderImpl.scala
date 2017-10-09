import com.mohiva.play.silhouette
import com.mohiva.play.silhouette.api.actions._
import com.mohiva.play.silhouette.api.crypto.Base64AuthenticatorEncoder
import com.mohiva.play.silhouette.api.util.{Clock, PasswordHasher, PasswordHasherRegistry, PlayHTTPLayer}
import com.mohiva.play.silhouette.api.{EventBus, SilhouetteProvider}
import com.mohiva.play.silhouette.crypto.JcaSigner
import com.mohiva.play.silhouette.impl.authenticators.JWTAuthenticatorService
import com.mohiva.play.silhouette.impl.providers._
import com.mohiva.play.silhouette.impl.providers.oauth2.{FacebookProvider, GoogleProvider}
import com.mohiva.play.silhouette.impl.providers.state.CsrfStateItemHandler
import com.mohiva.play.silhouette.impl.util.SecureRandomIDGenerator
import com.mohiva.play.silhouette.password.BCryptPasswordHasher
import com.mohiva.play.silhouette.persistence.repositories.DelegableAuthInfoRepository
import controllers._
import daos._
import filters.NoriginFilter
import models.auth.JWTEnv
import net.sf.ehcache.Cache
import play.api.Mode.{Dev, Prod, Test}
import play.api._
import play.api.cache.DefaultSyncCacheApi
import play.api.cache.ehcache.EhCacheApi
import play.api.libs.mailer.SMTPMailer
import play.api.libs.ws.ahc.AhcWSComponents
import play.api.mvc.{BodyParsers, EssentialFilter}
import play.api.routing.Router
import reactivemongo.api.{DefaultDB, MongoConnection, MongoDriver}
import router.Routes
import services.Mailer

import scala.concurrent.Future
import scala.language.postfixOps

/** This partially replaces the default application loader using Guice with a simpler one using constructor based DI, so
  * that wiring or configuration problems would cause failure at compile time instead of at runtime. */
class ApplicationLoaderImpl extends ApplicationLoader {
  override def load(context: ApplicationLoader.Context): Application = {
    LoggerConfigurator(context.environment.classLoader) foreach (_ configure context.environment)
    new Components(context).application
  }
}

class Components(context: ApplicationLoader.Context)
  extends BuiltInComponentsFromContext(context) with AssetsComponents with AhcWSComponents {
  // TODO: logging to Slack
//  scribe.Logger.root.clearHandlers()
//  scribe.Logger.root.addHandler(LogHandler(level = scribe.Level.Debug))
  scribe info "Reading application mode."
  val conf: Conf = context.environment.mode match {
    case Dev =>
      scribe info "Using dev configuration."
      Conf.devConf
    case Test =>
      scribe info "Using test configuration."
      Conf.testConf
    case Prod =>
      scribe info "Using prod configuration."
      Conf.prodConf
  }

  scribe info "Connecting to MongoDB."
  // TODO: handle failed connection:
  private val db: Future[DefaultDB] =
    MongoDriver() connection (MongoConnection parseURI conf.mongoHostPort get) database conf.dbName

  private val userDao: UserDao = new MongoUserDao(db)
  private val passwordInfoDao: PasswordInfoDao = new MongoPasswordInfoDao(db)
  private val oAuth2InfoDao: OAuth2InfoDao = new MongoOAuth2InfoDao(db)
  private val userTokenDao: UserTokenDao = new MongoUserTokenDao(db)
  private val charlistFeatureDao: CharlistFeatureDao = new MongoCharlistFeatureDao(db)
  private val charlistDao: CharlistDao = new MongoCharlistDao(db)
  private val picDao: PicDao = new MongoPicDao(db)

  private val bodyParsers = new BodyParsers.Default
  private val hTTPLayer = new PlayHTTPLayer(wsClient)
  private val iDGenerator = new SecureRandomIDGenerator
  private val signer = new JcaSigner(conf.jcaSignerSettings)
  private val socialStateHandler =
    new DefaultSocialStateHandler(Set(new CsrfStateItemHandler(conf.csrfStateSettings, iDGenerator, signer)), signer)
  private val authInfoRepository = new DelegableAuthInfoRepository(passwordInfoDao, oAuth2InfoDao)
  private val passwordHasher: PasswordHasher = new BCryptPasswordHasher
  private val silhouetteProvider = new SilhouetteProvider[JWTEnv](
    silhouette.api.Environment[JWTEnv](
      userDao,
      new JWTAuthenticatorService(conf.jwtAuthConf, None, new Base64AuthenticatorEncoder, iDGenerator, Clock()),
      Nil,
      EventBus()),
    new DefaultSecuredAction(
      new DefaultSecuredRequestHandler(new DefaultSecuredErrorHandler(messagesApi)),
      bodyParsers),
    new DefaultUnsecuredAction(
      new DefaultUnsecuredRequestHandler(new DefaultUnsecuredErrorHandler(messagesApi)),
      bodyParsers),
    new DefaultUserAwareAction(new DefaultUserAwareRequestHandler(), bodyParsers))

  private val mailer: Mailer = new Mailer(conf.mailerConf, new SMTPMailer(conf.smtpConf), messagesApi)
  private val authController = new AuthController(
    controllerComponents,
    silhouetteProvider,
    SocialProviderRegistry(new FacebookProvider(hTTPLayer, socialStateHandler, conf.facebookConf) ::
      new GoogleProvider(hTTPLayer, socialStateHandler, conf.googleConf) :: Nil),
    authInfoRepository,
    new Base64AuthenticatorEncoder,
    new CredentialsProvider(authInfoRepository, PasswordHasherRegistry(passwordHasher)),
    userDao,
    userTokenDao,
    passwordHasher,
    conf.authControllerConf,
    mailer)
  private val charlistFeatureController: CharlistFeatureController =
    new CharlistFeatureController(controllerComponents, silhouetteProvider, charlistFeatureDao)
  private val charlistController = new CharlistController(controllerComponents, silhouetteProvider, charlistDao, picDao)
  private val miscController: MiscController = new MiscController(
    controllerComponents,
    new DefaultSyncCacheApi(new EhCacheApi(new Cache(conf.cacheConf))),
    router = router)

  override lazy val router: Router = new Routes(
    httpErrorHandler,
    authController,
    charlistFeatureController,
    charlistController,
    miscController,
    assets)

  // TODO: add configuration for prod CORS filter:
  override lazy val httpFilters: Seq[EssentialFilter] = new NoriginFilter :: Nil
}
