module Bingo exposing (..)

import Html
import Html.Attributes
import Html.Events


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
    , Entry 4 "Rock-Star Ninja" 400 False
    , Entry 3 "In the Cloud" 300 False
    , Entry 2 "Doing Agile" 200 False
    ]



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | SortByPoints


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model
                | gameNumber = model.gameNumber + 1
                , entries = initialEntries
              }
            , Cmd.none
            )

        Mark entryId ->
            let
                switchSpokenStatus : Entry -> Entry
                switchSpokenStatus entry =
                    if entryId == entry.id then
                        { entry | marked = not entry.marked }
                    else
                        entry
            in
                ( { model | entries = List.map switchSpokenStatus model.entries }
                , Cmd.none
                )

        SortByPoints ->
            ( { model | entries = List.sortBy .points model.entries }
            , Cmd.none
            )



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


viewEntry : Entry -> Html.Html Msg
viewEntry entry =
    Html.li
        [ Html.Events.onClick (Mark entry.id)
        , Html.Attributes.classList [ ( "marked", entry.marked ) ]
        ]
        [ Html.span [ Html.Attributes.class "phrase" ] [ Html.text entry.phrase ]
        , Html.span [ Html.Attributes.class "points" ] [ Html.text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html.Html Msg
viewEntryList entries =
    Html.ul [] (List.map viewEntry entries)


viewNewGameButton : Html.Html Msg
viewNewGameButton =
    Html.div
        [ Html.Attributes.class "button-group" ]
        [ Html.button
            [ Html.Events.onClick NewGame ]
            [ Html.text "New Game" ]
        ]


viewSortButton : Html.Html Msg
viewSortButton =
    Html.div
        [ Html.Attributes.class "button-group" ]
        [ Html.button
            [ Html.Events.onClick SortByPoints ]
            [ Html.text "Sort" ]
        ]


totalPoints : List Entry -> Int
totalPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\entry acc -> acc + entry.points) 0


viewTotalPoints : List Entry -> Html.Html Msg
viewTotalPoints entries =
    Html.div
        [ Html.Attributes.class "score" ]
        [ Html.span
            [ Html.Attributes.class "label" ]
            [ Html.text "Score" ]
        , Html.span
            [ Html.Attributes.class "value" ]
            [ Html.text (entries |> totalPoints |> toString) ]
        ]


view : Model -> Html.Html Msg
view model =
    Html.div
        [ Html.Attributes.class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewTotalPoints model.entries
        , viewNewGameButton
        , viewSortButton
        , viewFooter
        ]


main : Program Never Model Msg
main =
    Html.program
        { init = ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
