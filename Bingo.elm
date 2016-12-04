module Bingo exposing (..)

import Html
import Html.Attributes


playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer name gameNumber =
    Html.h2
        [ Html.Attributes.id "info"
        , Html.Attributes.class "classy"
        ]
        [ Html.text (playerInfo name gameNumber) ]


viewHeader title =
    Html.header []
        [ Html.h1 [] [ Html.text title ] ]


viewFooter =
    Html.footer []
        [ Html.a
            [ Html.Attributes.href "http://elm-lang.org" ]
            [ Html.text "Powered By Elm" ]
        ]


view =
    Html.div
        [ Html.Attributes.class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer "David" 3
        , viewFooter
        ]


main =
    view
