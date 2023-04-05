module Main exposing (main)

import Browser
import Browser.Dom exposing (focus)
import Task exposing (attempt)
import Html exposing (..)
import Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Url exposing (Url)
import Url.Parser exposing (Parser, top, string, (</>), oneOf, parse)
import Url.Builder as Url
import Browser.Navigation as Nav
import Http
import Json.Decode as D
import Json.Encode as E
import Ports exposing (..)
import String exposing (fromInt)
import Tuple exposing (second)
import Array exposing (Array)
import Browser exposing (UrlRequest(..))


main : Program { xtoken : String, origin : String } Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "Anime RSS", body = view model }
        , update = update
        , subscriptions = \_ -> xTokenReceiver XTokenFetched
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

emptyLoginPage : Route -> Page
emptyLoginPage route = LoginPage <| LoginPageModel "" "" False route

initAnimeListPage : Array AnimeEntry -> Page
initAnimeListPage animes = AnimeListPage <| AnimeListPageModel "" All animes

loginCall : LoginPageModel -> Cmd Msg
loginCall loginData = Http.post {url = "/api/login", body = Http.jsonBody <| loginDataToJson loginData, expect = Http.expectWhatever <| Login << LoginCallFinised}

loginDataToJson : LoginPageModel -> E.Value
loginDataToJson loginData = E.object
    [ ("password", E.string loginData.password)
    , ("email", E.string loginData.email)
    ]

type LoginPageMsg =
      Email String
    | Password String
    | LoginClicked
    | LoginCallFinised (Result Http.Error ())

type alias LoginPageModel =
    { email : String
    , password : String
    , error: Bool
    , nextUrl : Route
    }

updateAnimeListPage : String -> AnimeListMsg -> AnimeListPageModel -> (AnimeListPageModel, Cmd Msg)
updateAnimeListPage xtoken animeListMsg animeListPageModel = case animeListMsg of
    Search newSearch -> ({ animeListPageModel | search = newSearch}, fetchAnimeListCmd (Just animeListPageModel.subscriptionSelector) (Just newSearch) xtoken)
    SelectorChanged newSelector -> ({animeListPageModel | subscriptionSelector = newSelector }, fetchAnimeListCmd (Just newSelector) (Just animeListPageModel.search) xtoken)
    ChangeAnimeSubscription index animeId follow -> (updateAnimeFollowing index follow animeListPageModel, followAnimeCmd { animeId = animeId, follow = follow} xtoken)

updateAnimeFollowing : Int -> Bool -> AnimeListPageModel -> AnimeListPageModel
updateAnimeFollowing index follow animeListPageModel =
    let mElem = Array.get index animeListPageModel.animes
        mNewElem = Maybe.map (\v -> { v |following = follow}) mElem
    in case mNewElem of
        Just newElem -> {animeListPageModel | animes = Array.set index newElem animeListPageModel.animes}
        Nothing -> animeListPageModel

updateLoginPage : LoginPageMsg -> LoginPageModel -> (LoginPageModel, Cmd Msg)
updateLoginPage loginMsg loginPageModel = case loginMsg of
    Email newEmail -> ({ loginPageModel | email = newEmail, error = False }, Cmd.none)
    Password newPassword -> ({ loginPageModel | password = newPassword, error = False }, Cmd.none)
    LoginClicked -> (loginPageModel,  loginCall <| loginPageModel)
    LoginCallFinised res -> case res of
        Ok _ -> ({ loginPageModel | email = "", password = "" }, Ports.fetchXToken ())
        -- TODO: Report login error.
        Err _ -> ({ loginPageModel | error = True} , Ports.fetchXToken ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case (msg, model.page) of
    (GotAnimeList res, _) -> resolveResponse model res resolveGotAnimeList
    (GotProfileData res, _) -> resolveResponse model res resolveGotProfile
    (AnimeList animeListAction, AnimeListPage animeListPageModel) -> updatePage model AnimeListPage <| updateAnimeListPage model.xtoken animeListAction animeListPageModel
    (Login loginFormAction, LoginPage loginPageModel) -> updatePage model LoginPage <| updateLoginPage loginFormAction loginPageModel
    (XTokenFetched xtoken, LoginPage loginPageModel) -> ({ model | xtoken = xtoken }, Nav.pushUrl model.key <| routeToHref <| loginPageModel.nextUrl)
    (XTokenFetched xtoken, _) -> ({ model | xtoken = xtoken }, Cmd.none)
    (MovingToPage nextRoute, _) -> updateMovingToPage model nextRoute
    (AskForXtoken _, _) -> (model, Ports.fetchXToken ())
    (MovingToExternalPage externalUrl, _) -> (model, Nav.load externalUrl)
    (_, _) -> (model, Cmd.none)

updatePage : Model -> (subModel -> Page) -> (subModel, Cmd Msg) -> (Model, Cmd Msg)
updatePage model toModel (subModel, cmd) = ({ model | page =  toModel subModel}, cmd)


resolveGotAnimeList : Model -> (Array AnimeEntry) -> (Model, Cmd Msg)
resolveGotAnimeList model v = case model.page of
    AnimeListPage animeListPageModel -> ({model | page = AnimeListPage {animeListPageModel | animes = v }}, Cmd.none)
    _ -> ({model | page = initAnimeListPage v }, Cmd.none)

resolveGotProfile : Model -> ProfileData -> (Model, Cmd Msg)
resolveGotProfile model v = ({model | page = ProfilePage v }, Cmd.none)

resolveResponse : Model -> Result Http.Error a -> (Model -> a -> (Model, Cmd Msg)) -> (Model, Cmd Msg)
resolveResponse model res f = case res of
    Err err -> case err of
        Http.BadStatus v -> case v of
            401 -> ({model | page = emptyLoginPage <| pageToRoute model.page}, focusLoginEmial)
            _ -> (model, Cmd.none)
        _ -> (model, Cmd.none)
    Ok v -> Tuple.mapSecond (\cmd -> Cmd.batch [cmd, fetchXToken ()]) <| f model v

type SubscriptionAction = Subscribe | Unsubscribe

subscriptionActionToString : SubscriptionAction -> String
subscriptionActionToString sub = case sub of
    Subscribe -> "sub"
    Unsubscribe -> "unsub"

parseSubscriptionAction : Parser (SubscriptionAction -> a) a
parseSubscriptionAction = oneOf
    [ Url.Parser.map Subscribe (Url.Parser.s "sub")
    , Url.Parser.map Unsubscribe (Url.Parser.s "unsub")
    ]

type NavBarElement msg =
      Link NavBarLink
    | Button (NavBarButton msg)

type alias NavBarLink =
    { text : String
    , link : String
    , selected : Bool
    }

type alias NavBarButton msg =
    { text : String
    , msg : msg
    }

viewNavBar : (List (NavBarElement Msg), List (NavBarElement Msg)) -> Html Msg
viewNavBar (left, right) =
    node "nav" []
      [ div [class "nav-div"]
        [ ul [class "left-submenu"] <| List.map viewNavBarElement left
        , ul [class "right-submenu"] <| List.map viewNavBarElement right
        ]
      ]

whenList : Bool -> a -> List a
whenList condition v = if condition
    then [v]
    else []

viewNavBarElement : NavBarElement msg -> Html msg
viewNavBarElement nbe =
    case nbe of
        Link nbl -> if nbl.selected
            then li (whenList nbl.selected (class "selected")) [text nbl.text]
            else li [] [a [href nbl.link] [text nbl.text]]
        Button nbb -> li [] [div [onClick nbb.msg] [text nbb.text]]

type Route =
      RootRoute
    | LoginRoute
    | MyProfileRoute
    | AnimeListRoute
    | AnimeSubscriptionRoute String SubscriptionAction

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ Url.Parser.map RootRoute top
    , Url.Parser.map LoginRoute (Url.Parser.s "login")
    , Url.Parser.map MyProfileRoute (Url.Parser.s "profile")
    , Url.Parser.map AnimeListRoute (Url.Parser.s "anime-list")
    , Url.Parser.map AnimeSubscriptionRoute (Url.Parser.s "anime" </> string </> parseSubscriptionAction)
    ]

pageToRoute : Page -> Route
pageToRoute page = case page of
    Loading -> AnimeListRoute
    ProfilePage _ -> MyProfileRoute
    LoginPage _ -> LoginRoute
    AnimeListPage _ -> AnimeListRoute

updateMovingToPage : Model -> Route -> (Model, Cmd Msg)
updateMovingToPage model route = case route of
    RootRoute -> (model, Nav.pushUrl model.key <| routeToHref AnimeListRoute)
    LoginRoute -> ({ model | page = emptyLoginPage AnimeListRoute}, Cmd.batch [Nav.pushUrl model.key <| routeToHref AnimeListRoute, focusLoginEmial])
    MyProfileRoute -> (model, Cmd.batch [fetchProfileCmd model.xtoken])
    AnimeListRoute -> ({ model | page = AnimeListPage <| AnimeListPageModel "" All Array.empty }, fetchAnimeListCmd Nothing Nothing model.xtoken )
    AnimeSubscriptionRoute _ _ -> (model, Cmd.none)

parseLocation : Url -> Route
parseLocation location =
    case (parse routeParser location) of
        Just route -> route
        Nothing -> RootRoute

routeToHref : Route -> String
routeToHref route = case route of
    RootRoute -> "/"
    LoginRoute -> "/login"
    MyProfileRoute -> "/profile"
    AnimeListRoute -> "/anime-list"
    (AnimeSubscriptionRoute id sub) -> "/anime/" ++ id ++ subscriptionActionToString sub

focusLoginEmial : Cmd Msg
focusLoginEmial = attempt (\_ -> NoOp) <| (focus "login-form-email-id")

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest urlRequest = case urlRequest of
    Internal url -> MovingToPage <| parseLocation url
    External externalUrl -> MovingToExternalPage externalUrl

onUrlChange : Url -> Msg
onUrlChange url = MovingToPage <| parseLocation url

getWithXtoken : { url : String, expect : Http.Expect msg, xtoken : String } -> Cmd msg
getWithXtoken r = Http.request
    { method = "GET"
    , headers = [Http.header "x-token" r.xtoken, Http.header "Accept" "application/json"]
    , url = r.url
    , body = Http.emptyBody
    , expect = r.expect
    , timeout = Nothing
    , tracker = Nothing
    }

postWithXtoken : { url : String, expect : Http.Expect msg, xtoken : String, body : E.Value} -> Cmd msg
postWithXtoken r = Http.request
    { method = "POST"
    , headers = [Http.header "x-token" r.xtoken, Http.header "Accept" "application/json"]
    , url = r.url
    , body = Http.jsonBody r.body
    , expect = r.expect
    , timeout = Nothing
    , tracker = Nothing
    }

-- The full application state of our todo app.
type alias Model =
    { xtoken : String
    , origin : String
    , page : Page
    , key : Nav.Key
    }

jsonDecodeUrl : D.Decoder Url
jsonDecodeUrl =
    let f text = case Url.fromString text of
            Just v -> D.succeed v
            Nothing -> D.fail "Not decodable Url"
    in D.andThen f D.string

animeDecode : D.Decoder (Array AnimeEntry)
animeDecode = D.array <| D.map6 AnimeEntry
    (D.field "title" D.string)
    (D.field "animeId" D.string)
    (D.field "url" jsonDecodeUrl)
    (D.field "imageUrl" jsonDecodeUrl)
    (D.field "date" D.string)
    (D.field "following" D.bool)

fetchAnimeListCmd : Maybe SubscriptionSelector -> Maybe String -> String -> Cmd Msg
fetchAnimeListCmd subscriptionSelector search xtoken =
    let url = Url.absolute ["api", "animes"] queryParams
        queryParams = (Maybe.withDefault [] <| Maybe.map (\v -> [Url.string "sub" <| subscriptionSelectorToString v]) subscriptionSelector)
          ++ (Maybe.withDefault [] <| Maybe.map (\v -> [Url.string "search" v]) search)
    in getWithXtoken
    { url = url
    , expect = Http.expectJson GotAnimeList animeDecode
    , xtoken = xtoken
    }

type alias AnimeFollowRequest =
    { animeId : String
    , follow : Bool
    }

animeFollowRequestEncode : AnimeFollowRequest -> E.Value
animeFollowRequestEncode data = E.object
    [ ("animeId", E.string data.animeId)
    , ("follow", E.bool data.follow)
    ]

followAnimeCmd : AnimeFollowRequest -> String -> Cmd Msg
followAnimeCmd followingRequest xtoken = postWithXtoken
    { url = "api/follow/anime"
    , expect = Http.expectWhatever AskForXtoken
    , xtoken = xtoken
    , body = animeFollowRequestEncode followingRequest
    }

type Page =
      Loading
    | ProfilePage ProfileData
    | LoginPage LoginPageModel
    | AnimeListPage AnimeListPageModel

type SubscriptionSelector =
      All
    | Subscribed
    | Unsubscribed

type alias AnimeEntry =
    { title : String
    , animeId : String
    , url : Url
    , imageUrl : Url
    , date : String
    , following : Bool
    }

type alias AnimeListPageModel =
    { search : String
    , subscriptionSelector : SubscriptionSelector
    , animes : Array AnimeEntry
    }

subscriptionSelectorToString : SubscriptionSelector -> String
subscriptionSelectorToString v = case v of
   All -> "All"
   Subscribed -> "Subscribed"
   Unsubscribed -> "Unsubscribed"

emptyModel : {xtoken: String, origin: String} -> Nav.Key -> Model
emptyModel flags key =
    { xtoken = flags.xtoken
    , origin = flags.origin
    , page = Loading
    , key = key
    }

init : {xtoken: String, origin: String} -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags _ key =
  ( emptyModel flags key
  , fetchAnimeListCmd Nothing Nothing flags.xtoken
  )

type AnimeListMsg =
      Search String
    | SelectorChanged SubscriptionSelector
    | ChangeAnimeSubscription Int String Bool

type Msg =
      GotAnimeList (Result Http.Error (Array AnimeEntry))
    | GotProfileData (Result Http.Error (ProfileData))
    | AskForXtoken (Result Http.Error ())
    | AnimeList AnimeListMsg
    | Login LoginPageMsg
    | XTokenFetched String
    | MovingToPage Route
    | MovingToExternalPage String
    | NoOp

viewLoadingPage : List (Html Msg)
viewLoadingPage =
    -- Loader downloaded from here: https://icons8.com/preloaders
    [div [class "loading"] [img [src "/loading.png"] []]]

loginPage : LoginPageModel -> List (Html Msg)
loginPage loginPageModel = [ loginForm {onSubmit = Login LoginClicked, onEmailInput = Login << Email, onPasswordInput = Login << Password} loginPageModel ]

loginForm : {onSubmit : Msg, onEmailInput : String -> Msg, onPasswordInput : String -> Msg} -> LoginPageModel -> Html Msg
loginForm config loginPageModel = let errorHtml = if loginPageModel.error then [ div [class "error-box"] [ text "Email or password you entered is incorrect." ]] else []
    in
    div [class "login-form-center"]
    [ Html.form [class "login-form-flexbox", onSubmit config.onSubmit]
        <| [ div [class <| "fancy-input-wrapper " ++ if loginPageModel.error then "fancy-input-wrapper-error" else ""]
            [ i [class "fa-sharp fa-solid fa-user"] []
            , input [id "login-form-email-id", type_ "text", class "fancy-button-input", tabindex 1, placeholder "Email", onInput config.onEmailInput, value loginPageModel.email, autofocus True] []
            ]
        , div [class <| "fancy-input-wrapper " ++ if loginPageModel.error then "fancy-input-wrapper-error" else ""]
            [ i [class "fa-sharp fa-solid fa-lock"] []
            , input [type_ "password", class "fancy-button-input", tabindex 2, placeholder "Password", onInput config.onPasswordInput, value loginPageModel.password] []
            ]
        ] ++ errorHtml ++
        [ div [class "login-button-wrapper"] [button [type_ "submit", tabindex 3] [text "Login"]]
        ]
    ]

view : Model -> List (Html Msg)
view model = case model.page of
    Loading -> viewLoadingPage
    ProfilePage profileData -> viewProfilePage model.origin profileData
    LoginPage loginPageModel -> loginPage loginPageModel
    AnimeListPage animeListPageModel -> viewAnimeListPage animeListPageModel

viewAnimeListPage : AnimeListPageModel -> List (Html Msg)
viewAnimeListPage animeListPageModel = viewNavBar (leftNavAnimeListPage, rightNavAnimeListPage) :: (contentWrapper <|
     (div [class "filter-header"] <| viewAnimeListFileters {search = animeListPageModel.search, subscriptionSelector = animeListPageModel.subscriptionSelector, onSearchInput = AnimeList << Search, onSelectorClick = AnimeList << SelectorChanged})
     :: second (Array.foldl (\anime (count, divs) -> (count + 1, viewAnime count (AnimeList << ChangeAnimeSubscription count anime.animeId) anime :: divs)) (0, []) animeListPageModel.animes))

-- This should be done differently but whatever..
leftNavAnimeListPage : List (NavBarElement Msg)
leftNavAnimeListPage =
    [ Link (NavBarLink "Profile" (routeToHref MyProfileRoute) False)
    , Link (NavBarLink "Anime List" "#" True)
    ]

rightNavAnimeListPage : List (NavBarElement Msg)
rightNavAnimeListPage =
    []

viewAnimeSelector : (SubscriptionSelector -> Msg) -> SubscriptionSelector -> List (Html Msg)
viewAnimeSelector onSelectorClick selector = case selector of
    All -> [button [type_ "button selected", disabled True, onClick <| onSelectorClick All] [text "All"], button [type_ "button", onClick <| onSelectorClick Subscribed] [text "Subscribed"], button [type_ "button", onClick <| onSelectorClick Unsubscribed] [text "Unsubscribed"]]
    Subscribed -> [button [type_ "button", onClick <| onSelectorClick All] [text "All"], button [type_ "button selected", disabled True, onClick <| onSelectorClick Subscribed] [text "Subscribed"], button [type_ "button", onClick <| onSelectorClick Unsubscribed] [text "Unsubscribed"]]
    Unsubscribed -> [button [type_ "button", onClick <| onSelectorClick All] [text "All"], button [type_ "button", onClick <| onSelectorClick Subscribed] [text "Subscribed"], button [type_ "button selected", disabled True, onClick <| onSelectorClick Unsubscribed] [text "Unsubscribed"]]

viewAnimeSearch : (String -> Msg) -> String -> List (Html Msg)
viewAnimeSearch onSearchInput search = List.singleton <| div [class "search"]
    [ input [class "button-input", type_ "text", placeholder "Search", value search, onInput onSearchInput] []
    , i [class "fa-sharp fa-solid fa-magnifying-glass"] []
    ]

viewAnimeListFileters : {search : String, subscriptionSelector : SubscriptionSelector, onSearchInput : (String -> Msg), onSelectorClick : (SubscriptionSelector -> Msg)} -> List (Html Msg)
viewAnimeListFileters v = viewAnimeSelector v.onSelectorClick v.subscriptionSelector ++ viewAnimeSearch v.onSearchInput v.search

viewAnime : Int -> (Bool -> Msg) -> AnimeEntry -> Html Msg
viewAnime index onSubscribeCheck animeEntry = div [class "anime-list-row"]
    [ img [src <| Url.toString animeEntry.imageUrl] []
    , div []
        [ div [class "title"] [text animeEntry.title]
        , div []
            [ input [class "skewed-subscribe-checkbox", type_ "checkbox", id <| fromInt index, checked animeEntry.following, onCheck onSubscribeCheck] []
            , label [class "skewed-subscribe-checkbox", for <| fromInt index]
                [ div [class "skewed-subscribe-checkbox"] [text <| if animeEntry.following then "Sbuscribed" else "Unsubscribed" ] ]
            ]
        ]
    ]

contentWrapper : List (Html Msg) -> List (Html Msg)
contentWrapper content = [div [class "content-wrapper"] [div [class "content"] content]]

type alias ProfileData =
    { userId : String
    , email : String
    , name : String
    , episodeChannel : String
    }

profileDecode : D.Decoder ProfileData
profileDecode = D.map4 ProfileData
    (D.field "userId" D.string)
    (D.field "email" D.string)
    (D.field "name" D.string)
    (D.field "episodeChannel" D.string)

fetchProfileCmd : String -> Cmd Msg
fetchProfileCmd xtoken =
    getWithXtoken
    { url = "api/user"
    , expect = Http.expectJson GotProfileData profileDecode
    , xtoken = xtoken
    }

viewProfilePage : String -> ProfileData -> List (Html Msg)
viewProfilePage origin profileData =
    viewNavBar (leftNavProfilePage, rightNavProfilePage)
    :: contentWrapper (viewProfilePageData origin profileData)

viewProfilePageData : String -> ProfileData -> List (Html Msg)
viewProfilePageData origin profileData =
    [ div [class "profile-data"]
        [ div [class "profile-data-row"]
            [ text <| "New anime RSS channel: ", Html.a [href "atom/animes/"] [text <| origin ++ "/atom/episodes"] ]
        , div [class "profile-data-row"]
            [ text <| "Episode RSS channel: ", Html.a [href <| "atom/episodes/" ++ profileData.episodeChannel] [text <| origin ++ "/atom/episodes" ++ profileData.episodeChannel] ]
        ]
    ]

-- This should be done differently but whatever..
leftNavProfilePage : List (NavBarElement Msg)
leftNavProfilePage =
    [ Link (NavBarLink "Profile" (routeToHref MyProfileRoute) True)
    , Link (NavBarLink "Anime List" (routeToHref AnimeListRoute) False)
    ]

rightNavProfilePage : List (NavBarElement Msg)
rightNavProfilePage =
    []
