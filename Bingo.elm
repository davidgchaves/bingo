module Bingo exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Random
import Http
import Json.Decode as Decode exposing (Decoder)


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
    , alertMessage : Maybe String
    }


initialModel : Model
initialModel =
    { name = "andrei"
    , gameNumber = 1
    , entries = []
    , alertMessage = Nothing
    }



-- UPDATE


type Msg
    = NewGame
    | Mark Int
    | SortByPoints
    | RandomNumber Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, fetchInitialEntries )

        RandomNumber generatedNumber ->
            ( { model | gameNumber = generatedNumber }, Cmd.none )

        NewEntries (Ok randomEntries) ->
            ( { model | entries = randomEntries }, Cmd.none )

        NewEntries (Err error) ->
            let
                message =
                    case error of
                        Http.NetworkError ->
                            "Are you sure the server is running?"

                        Http.Timeout ->
                            "Request timed out!"

                        Http.BadUrl url ->
                            "Invalid URL: " ++ url

                        Http.BadStatus response ->
                            case response.status.code of
                                401 ->
                                    "Unauthorized"

                                404 ->
                                    "Not found"

                                code ->
                                    "Error Code: " ++ (toString code)

                        Http.BadPayload message _ ->
                            "JSON Decoder Error: " ++ message
            in
                ( { model | alertMessage = Just message }, Cmd.none )

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

        CloseAlert ->
            ( { model | alertMessage = Nothing }, Cmd.none )



-- COMMANDS


generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate RandomNumber (Random.int 1 100)


fetchInitialEntries : Cmd Msg
fetchInitialEntries =
    Http.send NewEntries (Http.get "http://localhost:3000/random-entries" (Decode.list entryDecoder))



-- DECODERS


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4
        Entry
        (Decode.field "id" Decode.int)
        (Decode.field "phrase" Decode.string)
        (Decode.field "points" Decode.int)
        (Decode.succeed False)



-- VIEW


playerInfo : PlayerName -> GameNumber -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : PlayerName -> GameNumber -> Html a
viewPlayer name gameNumber =
    Html.h2
        [ Html.Attributes.id "info"
        , Html.Attributes.class "classy"
        ]
        [ Html.text (playerInfo name gameNumber) ]


viewHeader : String -> Html a
viewHeader title =
    Html.header []
        [ Html.h1 [] [ Html.text title ] ]


viewFooter : Html a
viewFooter =
    Html.footer []
        [ Html.a
            [ Html.Attributes.href "http://elm-lang.org" ]
            [ Html.text "Powered By Elm" ]
        ]


viewEntry : Entry -> Html Msg
viewEntry entry =
    Html.li
        [ Html.Events.onClick (Mark entry.id)
        , Html.Attributes.classList [ ( "marked", entry.marked ) ]
        ]
        [ Html.span [ Html.Attributes.class "phrase" ] [ Html.text entry.phrase ]
        , Html.span [ Html.Attributes.class "points" ] [ Html.text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    Html.ul [] (List.map viewEntry entries)


viewNewGameButton : Html Msg
viewNewGameButton =
    Html.div
        [ Html.Attributes.class "button-group" ]
        [ Html.button
            [ Html.Events.onClick NewGame ]
            [ Html.text "New Game" ]
        ]


viewSortButton : Html Msg
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


viewTotalPoints : List Entry -> Html Msg
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


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            Html.div [ Html.Attributes.class "alert" ]
                [ Html.span
                    [ Html.Attributes.class "close"
                    , Html.Events.onClick CloseAlert
                    ]
                    [ Html.text "X" ]
                , Html.text message
                ]

        Nothing ->
            Html.div [] [ Html.text "" ]


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.class "content" ]
        [ viewHeader "BUZZWORD BINGO"
        , viewAlertMessage model.alertMessage
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
        { init = ( initialModel, fetchInitialEntries )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
