module Routing exposing (parseLocation, Route(..))

import Url exposing (..)
import Url.Parser exposing (..)


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
    [ map Root top
    , map Login (s "login")
    , map MyProfile (s "profile")
    , map AnimeList (s "anime-list")
    , map toAnimeSubscription (s "anime" </> string </> string)
    ]

parseLocation : Url -> Route
parseLocation location =
    case (parse routeParser location) of
        Just route -> route
        Nothing -> NotFoundRoute

