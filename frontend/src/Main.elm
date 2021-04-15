module Main exposing (main)

{-| TodoMVC implemented in Elm, using plain HTML and CSS for rendering.

This application is broken up into three key parts:

  1. Model  - a full definition of the application's state
  2. Update - a way to step the application state forward
  3. View   - a way to visualize our application state with HTML

This clean division of concerns is a core part of Elm. You can read more about
this in <http://guide.elm-lang.org/architecture/index.html>
-}

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


main : Program Value Model Msg
main =
    Browser.application
        { init = init
        , view = \model -> { title = "Elm • TodoMVC", body = [view model] }
        , update = updateWithStorage
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }

type SubscriptionAction = Subscribe | Unsubscribe

type Route =
      Root
    | Login
    | MyProfile
    | AnimeList
    | AnimeSubscription String SubscriptionAction
    | NotFoundRoute

toAnimeSubscription : String -> String -> Route
toAnimeSubscription id subVal =
    case subVal of
        "sub" -> AnimeSubscription id Subscribe
        "unsub" -> AnimeSubscription id Unsubscribe
        _ -> NotFoundRoute

routeParser : Parser (Route -> a) a
routeParser = oneOf
    [ Url.Parser.map Root top
    , Url.Parser.map Login (Url.Parser.s "login")
    , Url.Parser.map MyProfile (Url.Parser.s "profile")
    , Url.Parser.map AnimeList (Url.Parser.s "anime-list")
    , Url.Parser.map toAnimeSubscription (Url.Parser.s "anime" </> string </> string)
    ]

parseLocation : Url -> Route
parseLocation location =
    case (parse routeParser location) of
        Just route -> route
        Nothing -> NotFoundRoute


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest _ = NoOp

onUrlChange : Url -> Msg
onUrlChange _ = NoOp

-- The full application state of our todo app.
type alias Model =
    { loginInfo : Maybe LoginInfo
    , profile : Maybe Profile
    , page: Page
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

type Page =
      HomePage
    | LoginPage
    | AnimeListPage (Array Anime)

type alias LoginInfo =
    { xtoken : String
    }

emptyModel : Model
emptyModel =
    { loginInfo = Nothing
    }

init : Value -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ _ _=
  ( emptyModel
  , Cmd.none
  )



-- UPDATE


{-| Users of our app can trigger messages by clicking and typing. These
messages are fed into the `update` function as they occur, letting us react
to them.
-}
type Msg = NoOp



-- How we update our Model on a given Msg?
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

view : Model -> Html Msg
view model =
    div
        [ class "todomvc-wrapper"
        , style "visibility" "hidden"
        ]
        [ section
            [ class "todoapp" ]
            [ ]
        , infoFooter
        ]
