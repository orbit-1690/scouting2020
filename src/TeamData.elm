module TeamData exposing (Model, Msg, Stations(..), init, nameCheck, station, stationToString, subscriptions, team, update, view)

import Colors exposing (blue, orange, sky, white)
import Element exposing (centerX, centerY, column, fill, padding, spacing, text, width)
import Element.Background as Background
import Element.Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (labelHidden, radio)
import GetMatch exposing (getMatch, unwrapToString)
import String


main : Program () Model Msg
main =
    Browser.sandbox
        { init = init <| Array.fromList GetMatch.matches
        , view = Element.layout [] << view
        , update = update
        }


type Msg
    = ScouterInput String
    | MatchInput String
    | Station Stations


type alias Model =
    { scouterName : String
    , match : Maybe Int
    , station : Stations
    , team : String
    }


team : Model -> String
team model =
    getMatch model.match <| stationToString model.station


station : Model -> String
station model =
    stationToString model.station


type Stations
    = Blue1
    | Blue2
    | Blue3
    | Red1
    | Red2
    | Red3
    | NotAStation


inputOption : GetMatch.AllianceColor -> GetMatch.StationNumber -> String -> Input.Option ( AllianceColor, StationNumber ) msg
inputOption allianceColor allianceNumber text =
    Input.option ( allianceColor, allianceNumber ) (Element.text text)


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color sky
        , padding 50
        , spacing 40
        , centerX
        , centerY
        , rounded 20
        ]
        [ textInput model.scouterName ScouterInput "Scouter's name"
        , radio
            [ padding 10
            , spacing 20
            , Font.size 60
            ]
            { onChange = Station
            , selected = Just model.station
            , label = Input.labelAbove [ Font.size 60, padding 10, spacing 20 ] (text "Which station?")
            , options =
                [ inputOption GetMatch.Blue GetMatch.One "Blue 1"
                , inputOption GetMatch.Blue GetMatch.Two "Blue 2"
                , inputOption GetMatch.Blue GetMatch.Three "Blue 3"
                , inputOption GetMatch.Red GetMatch.One "Red 1"
                , inputOption GetMatch.Red GetMatch.Two "Red 2"
                , inputOption GetMatch.Red GetMatch.Three "Red 3"
                ]
            }
        , textInput (unwrapToString model.match) MatchInput "Match number"
        , Element.el
            [ Background.color orange
            , width fill
            , center
            , rounded 10
            , Font.color white
            , Font.glow blue 5
            , Font.size 60
            , Font.family
                [ Font.external
                    { name = "Open Sans"
                    , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                    }
                ]
            ]
            (Element.text <| team model)
        ]


stationToString : Stations -> String
stationToString chosenStation =
    case chosenStation of
        Blue1 ->
            "Blue 1"

        Blue2 ->
            "Blue 2"

        Blue3 ->
            "Blue 3"

        Red1 ->
            "Red 1"

        Red2 ->
            "Red 2"

        Red3 ->
            "Red 3"

        NotAStation ->
            "none"


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.text
        [ Font.color sky
        , Font.size 60
        , rounded 10
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                }
            ]
        ]
        { onChange = nextButton
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = labelHidden modelValue
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScouterInput name ->
            { model | scouterName = name }

        Station chosenStation ->
            { model | station = chosenStation }

        MatchInput match ->
            { model | match = String.toInt match }


subscriptions : Sub Msg
subscriptions =
    Sub.none
