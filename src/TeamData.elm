module TeamData exposing (Model, Msg, currentMatch, getMatch, getTeam, getter, init, stationToString, update, view)

import Array exposing (Array)
import Browser
import Colors exposing (black, blue, orange, sky, white)
import Element exposing (centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, minimum, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (checkbox, labelHidden, radio)
import GetMatch exposing (AllianceColor, Match, StationNumber, TeamStation, getTeamNum)
import Html.Attributes exposing (style)
import Maybe.Extra exposing (unwrap)
import Result.Extra exposing (merge)
import String


type Msg
    = ScouterInput String
    | MatchInput String
    | Station TeamStation
    | TeamEdit Bool
    | TeamInput String
    | IsRematch Bool


type alias Model =
    { scouterName : String
    , matchNumber : String
    , station : Maybe TeamStation
    , team : Result String Int
    , matches : Array Match
    , teamEdit : Bool
    , isRematch : Bool
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
    , "isRematch"
        ++ ","
        ++ (if model.isRematch then
                "1"

            else
                "0"
           )
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
    Model "" "" Nothing (Err "Fill in the fields") matches False False


currentMatch : Model -> Model
currentMatch model =
    { model | matchNumber = String.fromInt <| 1 + (Maybe.withDefault 0 <| String.toInt model.matchNumber) }


optionWithColor : String -> Input.OptionState -> Element.Element msg
optionWithColor station option =
    el
        (case option of
            Input.Idle ->
                [ Font.color black ]

            Input.Focused ->
                [ Font.color black ]

            Input.Selected ->
                [ Font.color <| findColorOption station
                , Font.bold
                ]
        )
        (text station)


checkBox : Bool -> (Bool -> Msg) -> String -> Element.Element Msg
checkBox model msg label =
    checkbox [ Font.size 30 ]
        { onChange = msg
        , icon = Input.defaultCheckbox
        , checked = model
        , label = Input.labelRight [ Font.size 30 ] <| text label
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
        [ Element.paddingXY 0 80
        , spacing 50
        , height fill
        , centerX
        ]
        [ textInput model.scouterName ScouterInput "your name"
        , text "Station"
            |> el
                [ Font.size 80
                , Font.underline
                ]
        , radio
            [ Font.size 75
            , height fill
            , spacing 45
            , centerX
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
        , column [ spacing 30 ]
            [ textInput model.matchNumber MatchInput "Match number"
            , if model.teamEdit then
                textInput teamString TeamInput "edit team here"

              else
                Element.none
            , teamString
                |> text
                |> el
                    [ Background.color orange
                    , rounded 10
                    , center
                    , Font.semiBold
                    , Font.color black
                    , Font.glow Colors.white 1
                    , Font.size 70
                    , width fill
                    , htmlAttribute <| style "height" "26%"
                    , Font.family
                        [ Font.external
                            { name = "Open Sans"
                            , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
                            }
                        ]
                    ]
            , row [ width fill ]
                [ checkBox model.teamEdit TeamEdit "edit team number"
                , el [ width fill ] <| text ""
                , checkBox model.isRematch IsRematch "is this a rematch?"
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
    let
        _ =
            Debug.log "station: " <| unwrap "No station selected" (\( color, number ) -> String.join " " [ colorToString color, numberToString number ]) alliance
    in
    unwrap "No station selected" (\( color, number ) -> String.join " " [ colorToString color, numberToString number ]) alliance


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue msg name =
    Input.text
        [ Font.color black
        , Font.size 68
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
                    case String.filter Char.isDigit input of
                        "" ->
                            Err "Not a number"

                        stringOfInts ->
                            -- Will always return the ints
                            Ok <| Maybe.withDefault 0 <| String.toInt stringOfInts
            }

        IsRematch state ->
            { model | isRematch = state }
