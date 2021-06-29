module Main exposing (main)

import String exposing (fromInt)
import Debug
import Array
import Browser
import Browser.Dom as Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed as Keyed
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import Json.Decode exposing (Value)
import Url exposing (Url)
import Url.Parser exposing (Parser, top, string, (</>), oneOf, parse)
import Browser.Navigation as Nav
import Http
import Json.Decode as D


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "Anime RSS", body = view model }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

updateWithStorage : Msg -> Model -> (Model, Cmd Msg)
updateWithStorage msg model = case msg of
    FetchAnimeList -> Debug.log "FetchAnimeList" (model, Cmd.none)
--    GotAnimeList (Result Http.Error (List Anime)) -> (model, Cmd.none)
    GotAnimeList res -> case res of
        Err err -> case err of
            Http.BadStatus _ -> Debug.log "GotAnimeList:Http.BadStatus" ({model | page = LoginPage}, Cmd.none)
            _ -> Debug.log "GotAnimeList:_" (model, Cmd.none)
        Ok _ -> Debug.log "GotAnimeList" (model, Cmd.none)
    Logout ->  Debug.log "GotAnimeList" (model, Cmd.none)
    LogIn -> Debug.log "GotAnimeList" (model, Cmd.none)
    NoOp -> Debug.log "GotAnimeList" (model, Cmd.none)

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
    div [class "navbar"]
        [ ul [class "left-submenu"] <| List.map viewNavBarElement left
        , ul [class "right-submenu"] <| List.map viewNavBarElement right
        ]

whenList : Bool -> a -> List a
whenList condition v = if condition
    then [v]
    else []

viewNavBarElement : NavBarElement msg -> Html msg
viewNavBarElement nbe =
    case nbe of
        Link nbl -> li (whenList nbl.selected (class "navbar-selected"))
            [a [href nbl.link] [text nbl.text]]
        Button nbb -> li [] [div [onClick nbb.msg] [text nbb.text]]

type Route =
      Root
    | Login
    | MyProfile
    | AnimeList
    | AnimeSubscription String SubscriptionAction

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ Url.Parser.map Root top
    , Url.Parser.map Login (Url.Parser.s "login")
    , Url.Parser.map MyProfile (Url.Parser.s "profile")
    , Url.Parser.map AnimeList (Url.Parser.s "anime-list")
    , Url.Parser.map AnimeSubscription (Url.Parser.s "anime" </> string </> parseSubscriptionAction)
    ]

parseLocation : Url -> Route
parseLocation location =
    case (parse routeParser location) of
        Just route -> route
        Nothing -> Root

routeToHref : Route -> String
routeToHref route = case route of
    Root -> "/"
    Login -> "/login"
    MyProfile -> "/profile"
    AnimeList -> "/anime-list"
    (AnimeSubscription id sub) -> "/anime/" ++ id ++ subscriptionActionToString sub

onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ = NoOp

onUrlChange : Url -> Msg
onUrlChange _ = NoOp

-- The full application state of our todo app.
type alias Model =
    { loginInfo : Maybe LoginInfo
    , profile : Maybe Profile
    , page : Page
    , animeList : Maybe Anime
    }

type alias Profile =
    { email : String
    , newAnimeChannel : String
    , newEpisodesChannel : String
    }

type alias Anime =
    { title : String
    , image : Url
    , followed : Bool
    }

jsonDecodeUrl : D.Decoder Url
jsonDecodeUrl =
    let f text = case Url.fromString text of
            Just v -> D.succeed v
            Nothing -> D.fail "Not decodable Url"
    in D.andThen f D.string

animeDecode : D.Decoder (List Anime)
animeDecode = D.list <| D.map3 Anime
    (D.field "title" D.string)
    (D.field "image" jsonDecodeUrl)
    (D.field "followed" D.bool)

fetchAnimeListCmd : Cmd Msg
fetchAnimeListCmd = Http.get
    { url = "/anime_list"
    , expect = Http.expectJson GotAnimeList animeDecode
    }

type Page =
      LoadingPage
    | ProfilePage
    | LoginPage
    | AnimeListPage

type alias LoginInfo =
    { xtoken : String
    }

emptyModel : Model
emptyModel =
    { loginInfo = Nothing
    , profile = Nothing
    , page = LoadingPage
    , animeList = Nothing
    }

init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _=
  ( emptyModel
  , fetchAnimeListCmd
  )


type Msg =
      FetchAnimeList
    | GotAnimeList (Result Http.Error (List Anime))
    | Logout
    | LogIn
    | NoOp


leftNav : List (NavBarElement Msg)
leftNav =
    [ Link (NavBarLink "Profile" "#" False)
    , Link (NavBarLink "Anime List" "#" True)
    ]

rightNav : List (NavBarElement Msg)
rightNav =
    [ Button (NavBarButton "Logout" Logout)
    ]

viewLoadingPage : List (Html Msg)
viewLoadingPage =
    -- Loader downloaded from here: https://icons8.com/preloaders
    [div [class "loading"] [img [src "/loading.png"] []]]

view : Model -> List (Html Msg)
view model = case model.page of
    LoadingPage -> viewLoadingPage
    ProfilePage ->
        [ viewNavBar (leftNav, rightNav)
        ]
    LoginPage -> []
    AnimeListPage -> []
