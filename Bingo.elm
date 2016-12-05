module Bingo exposing (..)

import Html
import Html.Attributes


-- MODEL


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias PlayerName =
    String


type alias GameNumber =
    Int


type alias Model =
    { name : PlayerName
    , gameNumber : GameNumber
    , entries : List Entry
    }


initialModel : Model
initialModel =
    { name = "andrei"
    , gameNumber = 1
    , entries = initialEntries
    }


initialEntries : List Entry
initialEntries =
    [ Entry 1 "Future-Proof" 100 False
    , Entry 2 "Doing Agile" 200 False
    , Entry 3 "In the Cloud" 300 False
    , Entry 4 "Rock-Star Ninja" 400 False
    ]



-- VIEW


playerInfo : PlayerName -> GameNumber -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : PlayerName -> GameNumber -> Html.Html a
viewPlayer name gameNumber =
    Html.h2
        [ Html.Attributes.id "info"
        , Html.Attributes.class "classy"
        ]
        [ Html.text (playerInfo name gameNumber) ]


viewHeader : String -> Html.Html a
viewHeader title =
    Html.header []
        [ Html.h1 [] [ Html.text title ] ]


viewFooter : Html.Html a
viewFooter =
    Html.footer []
        [ Html.a
            [ Html.Attributes.href "http://elm-lang.org" ]
            [ Html.text "Powered By Elm" ]
        ]


viewEntry : Entry -> Html.Html a
viewEntry entry =
    Html.li []
        [ Html.span [ Html.Attributes.class "phrase" ] [ Html.text entry.phrase ]
        , Html.span [ Html.Attributes.class "points" ] [ Html.text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html.Html a
viewEntryList entries =
    Html.ul [] (List.map viewEntry entries)


view : Model -> Html.Html a
view model =
    Html.div
        [ Html.Attributes.class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewFooter
        ]


main : Html.Html a
main =
    view initialModel
