import com.google.inject.{AbstractModule, Provides}
import com.mohiva.play.silhouette.api.crypto.{Base64AuthenticatorEncoder, Signer}
import com.mohiva.play.silhouette.api.repositories.AuthInfoRepository
import com.mohiva.play.silhouette.api.services.{AuthenticatorService, IdentityService}
import com.mohiva.play.silhouette.api.util.{IDGenerator, _}
import com.mohiva.play.silhouette.api.{Environment, EventBus, Silhouette, SilhouetteProvider}
import com.mohiva.play.silhouette.crypto.{JcaSigner, JcaSignerSettings}
import com.mohiva.play.silhouette.impl.authenticators.{JWTAuthenticator, JWTAuthenticatorService, JWTAuthenticatorSettings}
import com.mohiva.play.silhouette.impl.providers._
import com.mohiva.play.silhouette.impl.providers.oauth2.{FacebookProvider, GoogleProvider}
import com.mohiva.play.silhouette.impl.providers.state.{CsrfStateItemHandler, CsrfStateSettings}
import com.mohiva.play.silhouette.impl.util.{DefaultFingerprintGenerator, SecureRandomIDGenerator}
import com.mohiva.play.silhouette.password.BCryptPasswordHasher
import com.mohiva.play.silhouette.persistence.daos.DelegableAuthInfoDAO
import com.mohiva.play.silhouette.persistence.repositories.DelegableAuthInfoRepository
import daos._
import models.auth.{JWTEnv, User}
import net.ceedubs.ficus.Ficus._
import net.ceedubs.ficus.readers.ArbitraryTypeReader.arbitraryTypeValueReader
import net.ceedubs.ficus.readers.EnumerationReader.enumerationValueReader
import net.ceedubs.ficus.readers.ValueReader
import net.codingwell.scalaguice.ScalaModule
import play.api.Configuration
import play.api.libs.mailer.{MailerClient, SMTPConfiguration, SMTPMailer}
import play.api.libs.ws.WSClient
import play.api.mvc.{ControllerComponents, DefaultControllerComponents}

import scala.concurrent.ExecutionContext

class Module extends AbstractModule with ScalaModule {
  private implicit val enumReader: ValueReader[_] = enumerationValueReader
  private implicit val arbReaderOA2S: ValueReader[OAuth2Settings] = arbitraryTypeValueReader[OAuth2Settings]
  private implicit val arbReaderJWTAS: ValueReader[JWTAuthenticatorSettings] =
    arbitraryTypeValueReader[JWTAuthenticatorSettings]

  override def configure(): Unit = {
    bind[ControllerComponents].to[DefaultControllerComponents]
    bind[Silhouette[JWTEnv]].to[SilhouetteProvider[JWTEnv]]
    bind[PasswordHasher] toInstance new BCryptPasswordHasher
    bind[IdentityService[User]].to[UserDao]
    bind[DelegableAuthInfoDAO[PasswordInfo]].to[PasswordInfoDao]
    bind[DelegableAuthInfoDAO[OAuth2Info]].to[OAuth2InfoDao]
    bind[EventBus] toInstance EventBus()
    bind[Base64AuthenticatorEncoder] toInstance new Base64AuthenticatorEncoder
    bind[Clock] toInstance Clock()
    bind[FingerprintGenerator] toInstance new DefaultFingerprintGenerator(false)
    bind[MailerClient].to[SMTPMailer]
  }

  @Provides
  def provideEnvironment(
                          identityService: IdentityService[User],
                          authenticatorService: AuthenticatorService[JWTAuthenticator],
                          eventBus: EventBus)
                        (implicit ec: ExecutionContext): Environment[JWTEnv] =
    Environment[JWTEnv](identityService, authenticatorService, Seq(), eventBus)

  @Provides
  def provideHTTPLayer(client: WSClient)(implicit ec: ExecutionContext): HTTPLayer = new PlayHTTPLayer(client)

  @Provides
  def idGenerator()(implicit ec: ExecutionContext): IDGenerator = new SecureRandomIDGenerator

  @Provides
  def provideSocialProviderRegistry(
                                     facebookProvider: FacebookProvider,
                                     googleProvider: GoogleProvider): SocialProviderRegistry =
    SocialProviderRegistry(Seq(facebookProvider, googleProvider))

  @Provides
  def provideFacebookProvider(
                               hTTPLayer: HTTPLayer,
                               socialStateHandler: SocialStateHandler,
                               configuration: Configuration): FacebookProvider = new FacebookProvider(
    hTTPLayer,
    socialStateHandler,
    configuration.underlying as[OAuth2Settings] "silhouette.facebook")

  @Provides
  def provideGoogleProvider(
                             hTTPLayer: HTTPLayer,
                             socialStateHandler: SocialStateHandler,
                             configuration: Configuration): GoogleProvider = new GoogleProvider(
    hTTPLayer,
    socialStateHandler,
    configuration.underlying as[OAuth2Settings] "silhouette.google")

  @Provides
  def socialStateHandler(csrfStateItemHandler: CsrfStateItemHandler, signer: Signer): SocialStateHandler =
    new DefaultSocialStateHandler(Set(csrfStateItemHandler), signer)

  @Provides
  def provideSigner(configuration: Configuration): Signer =
    new JcaSigner(JcaSignerSettings(configuration.underlying getString "silhouette.authenticator.sharedSecret"))

  @Provides
  def provideAuthenticatorService(
                                   authenticatorEncoder: Base64AuthenticatorEncoder,
                                   idGenerator: IDGenerator,
                                   configuration: Configuration,
                                   clock: Clock)
                                 (implicit ec: ExecutionContext): AuthenticatorService[JWTAuthenticator] =
    new JWTAuthenticatorService(
      configuration.underlying as[JWTAuthenticatorSettings] "silhouette.authenticator",
      None,
      authenticatorEncoder,
      idGenerator,
      clock)

  @Provides
  def provideAuthInfoRepository(
                                 passwordInfoDAO: DelegableAuthInfoDAO[PasswordInfo],
                                 oauth2InfoDAO: DelegableAuthInfoDAO[OAuth2Info])
                               (implicit ec: ExecutionContext): AuthInfoRepository =
    new DelegableAuthInfoRepository(passwordInfoDAO, oauth2InfoDAO)

  @Provides
  def providePasswordHasherRegistry(passwordHasher: PasswordHasher): PasswordHasherRegistry =
    PasswordHasherRegistry(passwordHasher)

  @Provides
  def csrfStateSettings(configuration: Configuration): CsrfStateSettings =
    configuration.underlying as[CsrfStateSettings] "silhouette.csrfStateSettings"

  @Provides
  def smtpConfiguration(configuration: Configuration): SMTPConfiguration =
    SMTPConfiguration(configuration.underlying getConfig "play.mailer")
}
