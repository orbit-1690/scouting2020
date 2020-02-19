module TeamData exposing (Model, Msg, getMatch, getTeam, getter, init, stationToString, update, view)

import Array exposing (Array)
import Browser
import Colors exposing (black, blue, orange, sky, white)
import Element exposing (centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, minimum, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (checkbox, labelHidden, radio)
import GetMatch exposing (AllianceColor, Match, StationNumber, TeamStation, getTeamNum)
import Html.Attributes exposing (style)
import Maybe.Extra exposing (unwrap)
import Result.Extra exposing (merge)
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
    | Station TeamStation
    | TeamEdit Bool
    | TeamInput Int


type alias Model =
    { scouterName : String
    , matchNumber : String
    , station : Maybe TeamStation
    , team : Result String Int
    , matches : Array Match
    , teamEdit : Bool
    }


findColorOption : String -> Element.Color
findColorOption alliance =
    if String.contains "Blue" alliance then
        Colors.backgroundBlue

    else
        Colors.backgroundRed


getter : Model -> List String
getter model =
    [ "match" ++ "," ++ model.matchNumber
    , "station" ++ "," ++ stationToString model.station
    , "team"
        ++ ","
        ++ (getTeam model
                |> Result.map String.fromInt
                |> merge
           )
    , "scouterName" ++ "," ++ "'" ++ model.scouterName ++ "'"
    ]


init : Array Match -> Model
init matches =
    Model "" "" Nothing (Err "") matches False


optionWithColor : String -> Input.OptionState -> Element.Element msg
optionWithColor station option =
    el
        (case option of
            Input.Idle ->
                [ Font.color black, centerX ]

            Input.Focused ->
                [ Font.color black ]

            Input.Selected ->
                [ Font.color <| findColorOption station
                , Font.bold
                , centerX
                ]
        )
        (text station)


checkBox : Bool -> Element.Element Msg
checkBox model =
    checkbox [ Font.size 30 ]
        { onChange = TeamEdit
        , icon = Input.defaultCheckbox
        , checked = model
        , label = Input.labelRight [ Font.size 30 ] <| text "edit team number"
        }


inputOption : GetMatch.AllianceColor -> GetMatch.StationNumber -> String -> Input.Option ( AllianceColor, StationNumber ) msg
inputOption allianceColor allianceNumber text =
    Input.optionWith ( allianceColor, allianceNumber ) (optionWithColor text)


view : Model -> Element.Element Msg
view model =
    let
        teamString : String
        teamString =
            getTeam model
                |> Result.map String.fromInt
                |> merge
    in
    column
        [ Element.paddingXY 0 100
        , spacing 50
        , height fill
        , centerX
        ]
        [ textInput model.scouterName ScouterInput "your name"
        , text "Station"
            |> el
                [ Font.size 85
                , Font.underline
                , centerX
                , centerY
                ]
        , radio
            [ Font.size 80
            , height fill
            , spacing 70
            , centerX
            , center
            , width fill
            ]
            { onChange = Station
            , selected = model.station
            , label = Input.labelHidden ""
            , options =
                [ inputOption GetMatch.Blue GetMatch.One "Blue 1"
                , inputOption GetMatch.Blue GetMatch.Two "Blue 2"
                , inputOption GetMatch.Blue GetMatch.Three "Blue 3"
                , inputOption GetMatch.Red GetMatch.One "Red 1"
                , inputOption GetMatch.Red GetMatch.Two "Red 2"
                , inputOption GetMatch.Red GetMatch.Three "Red 3"
                ]
            }
        , textInput model.matchNumber MatchInput "Match number"
        , checkBox model.teamEdit
        , if model.teamEdit then
            textInput teamString (TeamInput << Maybe.withDefault 0 << String.toInt) "edit team here"

          else
            teamString
                |> text
                |> el
                    [ Background.color orange
                    , rounded 10
                    , centerX
                    , Font.semiBold
                    , Font.color black
                    , Font.glow Colors.white 1
                    , Font.size 70
                    , Font.family
                        [ Font.external
                            { name = "Open Sans"
                            , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
                            }
                        ]
                    ]
        ]


getTeam : Model -> Result String Int
getTeam model =
    if model.teamEdit then
        model.team

    else
        getMatch model
            |> Result.andThen
                (\match ->
                    model.station
                        |> Result.fromMaybe "No station"
                        |> Result.map
                            (\station ->
                                getTeamNum station match
                            )
                )


getMatch : Model -> Result String Match
getMatch { matchNumber, matches } =
    String.toInt matchNumber
        |> Result.fromMaybe "Invalid match number"
        |> Result.andThen
            (\number ->
                Array.get (number - 1) matches
                    |> Result.fromMaybe "No such match"
            )


colorToString : AllianceColor -> String
colorToString chosenColor =
    case chosenColor of
        GetMatch.Blue ->
            "Blue"

        GetMatch.Red ->
            "Red"


numberToString : StationNumber -> String
numberToString chosenNumber =
    case chosenNumber of
        GetMatch.One ->
            "1"

        GetMatch.Two ->
            "2"

        GetMatch.Three ->
            "3"


stationToString : Maybe TeamStation -> String
stationToString alliance =
    unwrap "No station selected" (\( color, number ) -> String.join " " [ colorToString color, numberToString number ]) alliance


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue msg name =
    Input.text
        [ Font.color black
        , Font.size 70
        , rounded 10
        ]
        { onChange = msg
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = Input.labelHidden modelValue
        }


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScouterInput name ->
            { model | scouterName = name }

        Station chosenStation ->
            { model
                | station = Just chosenStation
                , team = getTeam model
            }

        MatchInput matchNumber ->
            { model
                | matchNumber = matchNumber
                , team = getTeam model
            }

        TeamEdit state ->
            { model | teamEdit = state }

        TeamInput input ->
            { model
                | team =
                    Just input
                        |> Result.fromMaybe "Not a number"
            }
