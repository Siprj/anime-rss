module Page.AnimeList exposing (Anime, Model, view)

import Array exposing (Array)
import Html exposing (Html, div, button, text, img, input, label)
import Html.Attributes exposing (class, id, type_, for, disabled, src)


type Msg
    = Subscribe Int
    | Unsubscribe Int

type alias Model = Array Anime

type alias Anime =
    { title : String
    , thumbnailUrl : String
    , subscribed : Bool
    , id : String
    }

type alias Params msg =
    { embed : Msg -> msg }

view : Params msg -> Model -> Html msg
view { embed } animeList =
    div [class "content"]
        [ div [class "filter-header"]
            [ button [type_ "button", class "selected", disabled True] [text "all"]
            , button [type_ "button"] [text "asdf"]
            ]
        , div [class "list-content"]
            [ div [class "anime-list-row"]
                [ img [src "https://gogocdn.net/cover/sabiiro-no-armor-reimei.png"] []
                , div []
                    [ div [class "title"] [text "Sabiiro no Armor: Reimei"]
                    , div [class "skewed-subscribe-checkbox", type_ "checkbox"]
                        [ input [class "skewed-subscribe-checkbox", type_ "checkbox", id "bla"] []
                        , label [class "skewed-subscribe-checkbox", for "bla"]
                            [ div [class "skewed-subscribe-checkbox"] [text "Sbuscribed"] ]
                        ]
                    ]
                ]
            , div [class "anime-list-row"]
                [ img [src "https://gogocdn.net/cover/sabiiro-no-armor-reimei.png"] []
                , div []
                    [ div [class "title"] [text "Sabiiro no Armor: Reimei"]
                    , div [class "skewed-subscribe-checkbox", type_ "checkbox"]
                        [ input [class "skewed-subscribe-checkbox", type_ "checkbox", id "bla"] []
                        , label [class "skewed-subscribe-checkbox", for "bla"]
                            [ div [class "skewed-subscribe-checkbox"] [text "Sbuscribed"] ]
                        ]
                    ]
                ]
            ]
        ]
