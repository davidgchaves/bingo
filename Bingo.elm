module Bingo exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Random
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode


-- MODEL


type alias Entry =
    { id : Int
    , phrase : String
    , points : Int
    , marked : Bool
    }


type alias Score =
    { id : Int
    , name : String
    , value : Int
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
    | ShareScore
    | NewScore (Result Http.Error Score)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewGame ->
            ( { model | gameNumber = model.gameNumber + 1 }, fetchInitialEntries )

        ShareScore ->
            ( model, postScore model )

        RandomNumber generatedNumber ->
            ( { model | gameNumber = generatedNumber }, Cmd.none )

        NewScore (Ok score) ->
            let
                message =
                    "Your score of "
                        ++ (toString score.value)
                        ++ " was successfully shared!"
            in
                ( { model | alertMessage = Just message }, Cmd.none )

        NewScore (Err error) ->
            let
                message =
                    "Error posting your score: " ++ (toString error)
            in
                ( { model | alertMessage = Just message }, Cmd.none )

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
    let
        randomEntriesUrl =
            "http://localhost:3000/random-entries"

        request =
            Http.get randomEntriesUrl entriesDecoder
    in
        Http.send NewEntries request


postScore : Model -> Cmd Msg
postScore model =
    let
        scoresUrl =
            "http://localhost:3000/scores"

        body =
            model |> scoreEncoder |> Http.jsonBody

        request =
            Http.post scoresUrl body scoreDecoder
    in
        Http.send NewScore request



-- DECODERS / ENCODERS


scoreEncoder : Model -> Encode.Value
scoreEncoder model =
    Encode.object
        [ ( "name", Encode.string model.name )
        , ( "value", Encode.int (totalPoints model.entries) )
        ]


scoreDecoder : Decoder Score
scoreDecoder =
    Decode.map3
        Score
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "value" Decode.int)


entryDecoder : Decoder Entry
entryDecoder =
    Decode.map4
        Entry
        (Decode.field "id" Decode.int)
        (Decode.field "phrase" Decode.string)
        (Decode.field "points" Decode.int)
        (Decode.succeed False)


entriesDecoder : Decoder (List Entry)
entriesDecoder =
    Decode.list entryDecoder



-- VIEW


playerInfo : PlayerName -> GameNumber -> String
playerInfo name gameNumber =
    name ++ " - Game #" ++ (toString gameNumber)


viewPlayer : PlayerName -> GameNumber -> Html a
viewPlayer name gameNumber =
    Html.h2
        [ Html.Attributes.id "info"
        , class "classy"
        ]
        [ Html.text (playerInfo name gameNumber) ]


viewHeader : Html a
viewHeader =
    Html.header []
        [ Html.h1 [] [ Html.text "BUZZWORD BINGO" ] ]


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
        [ onClick (Mark entry.id)
        , classList [ ( "marked", entry.marked ) ]
        ]
        [ Html.span [ class "phrase" ] [ Html.text entry.phrase ]
        , Html.span [ class "points" ] [ Html.text (toString entry.points) ]
        ]


viewEntryList : List Entry -> Html Msg
viewEntryList entries =
    Html.ul [] (List.map viewEntry entries)


viewGameButtons : Html Msg
viewGameButtons =
    Html.div
        [ class "button-group" ]
        [ Html.button [ onClick NewGame ] [ Html.text "New Game" ]
        , Html.button [ onClick ShareScore ] [ Html.text "Share Score" ]
        ]


viewSortButton : Html Msg
viewSortButton =
    Html.div
        [ class "button-group" ]
        [ Html.button [ onClick SortByPoints ] [ Html.text "Sort" ] ]


totalPoints : List Entry -> Int
totalPoints entries =
    entries
        |> List.filter .marked
        |> List.foldl (\entry acc -> acc + entry.points) 0


viewTotalPoints : List Entry -> Html Msg
viewTotalPoints entries =
    Html.div
        [ class "score" ]
        [ Html.span [ class "label" ] [ Html.text "Score" ]
        , Html.span [ class "value" ] [ Html.text (entries |> totalPoints |> toString) ]
        ]


viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
    case alertMessage of
        Just message ->
            Html.div [ class "alert" ]
                [ Html.span
                    [ class "close"
                    , onClick CloseAlert
                    ]
                    [ Html.text "X" ]
                , Html.text message
                ]

        Nothing ->
            Html.div [] [ Html.text "" ]


view : Model -> Html Msg
view model =
    Html.div
        [ class "content" ]
        [ viewHeader
        , viewAlertMessage model.alertMessage
        , viewPlayer model.name model.gameNumber
        , viewEntryList model.entries
        , viewTotalPoints model.entries
        , viewGameButtons
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
