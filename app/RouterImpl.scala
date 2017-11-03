import controllers._
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._

class RouterImpl(
    authCtr: AuthController,
    charlistCtr: CharlistController,
    clFeatureCtr: CharlistFeatureController,
    miscCtr: MiscController,
    apiCtr: ApiHelpController,
    default: Default,
    assets: Assets) extends SimpleRouter {
  def routes: Routes = {
    case POST(p"/api/auth/signup")          => authCtr signUp()
    case GET(p"/api/auth/signup/$token")    => authCtr signUpConfirm token
    case POST(p"/api/auth/signin")          => authCtr signIn()
    case GET(p"/api/auth/social/$provider") => authCtr social provider
    case GET(p"/api/auth/signout")          => authCtr signOut()
    case POST(p"/api/auth/reset")           => authCtr resetPassword()
    case POST(p"/api/auth/reset/$token")    => authCtr resetPasswordConfirm token
    case POST(p"/api/auth/change")          => authCtr changePassword()

    case GET(p"/api/characters")         => charlistCtr list()
    case POST(p"/api/characters")        => charlistCtr add()
    case GET(p"/api/characters/default") => charlistCtr create()
    case GET(p"/api/characters/$id/pic") => charlistCtr getPic id
    case PUT(p"/api/characters/$id/pic") => charlistCtr storePic id
    case GET(p"/api/characters/$id")     => charlistCtr get id
    case PUT(p"/api/characters/$id")     => charlistCtr replace id
    case PATCH(p"/api/characters/$id")   => charlistCtr update id
    case DELETE(p"/api/characters/$id")  => charlistCtr delete id

    case POST(p"/api/features/")                                         => clFeatureCtr add()
    case GET(p"/api/features/search" ? q_*"col=$cols" & q_?"term=$term") => clFeatureCtr search(cols, term)
    case GET(p"/api/features/$col/new")                                  => clFeatureCtr create col
    case GET(p"/api/features/$col/" & q_?"term=$term")                   => clFeatureCtr search(col :: Nil, term)
    case GET(p"/api/features/$id")                                       => clFeatureCtr get id

    case OPTIONS(p"/api/$url*") => miscCtr options url

    case GET(p"/api")     => default redirect "/swagger?url=api.yml"
    case GET(p"/swagger") => assets at(path = "/public/lib/swagger-ui", file = "index.html")
    case GET(p"/api.yml") => apiCtr getResources()
    case GET(p"/$file*")  => assets at(path = "/public/lib/swagger-ui", file)
  }
}
