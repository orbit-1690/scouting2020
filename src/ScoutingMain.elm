module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous
import Browser
import Browser.Events as BE
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (Color, centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, layout, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import File.Download as Download
import GetMatch exposing (stationToString)
import Html.Attributes exposing (style)
import Result.Extra exposing (merge)
import TeamData
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout [ width fill, height fill, htmlAttribute <| style "touch-action" "manipulation" ]
        , update = update
        , subscriptions = always Sub.none
        }


widthPrecent : Int -> Element.Attribute Msg
widthPrecent precent =
    htmlAttribute << style "width" <| String.fromInt precent ++ "%"


heightPrecent : Int -> Element.Attribute Msg
heightPrecent precent =
    htmlAttribute << style "height" <| String.fromInt precent ++ "%"


type Pages
    = TeamDataPage
    | AutonomousPage
    | TeleopPage
    | ClimbingPage


type Msg
    = TeamDataMsg TeamData.Msg
    | AutonomousDataMsg Autonomous.Msg
    | TeleopDataMsg Teleop.Msg
    | ClimbingDataMsg Climbing.Msg
    | PrevPage
    | NextPage
    | Submit


type PagePosition
    = FirstPage
    | MiddlePage
    | LastPage


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    }


findColor : String -> Element.Color
findColor alliance =
    if String.contains "Blue" alliance then
        Colors.backgroundBlue

    else if String.contains "Red" alliance then
        Colors.backgroundRed

    else
        purple


stylishPage : PagePosition -> Element.Element Msg
stylishPage position =
    (case position of
        FirstPage ->
            button
                buttonStyle
                { onPress = Just <| NextPage
                , label = Element.text "Next"
                }

        LastPage ->
            Element.row
                [ spacing 15, centerX ]
                [ button
                    buttonStyle
                    { onPress = Just <| PrevPage
                    , label = Element.text "Previous"
                    }
                , button
                    buttonStyle
                    { onPress = Just <| Submit
                    , label = Element.text "Submit"
                    }
                ]

        MiddlePage ->
            Element.row
                [ spacing 100, centerX ]
                [ button
                    buttonStyle
                    { onPress = Just <| PrevPage
                    , label = Element.text "Previous"
                    }
                , button
                    buttonStyle
                    { onPress = Just <| NextPage
                    , label = Element.text "Next"
                    }
                ]
    )
        |> el
            [ centerX
            , centerY
            , width fill
            , heightPrecent 100
            ]


init : Model
init =
    { teamData = TeamData.init <| Array.fromList GetMatch.matches
    , autonomousData = Autonomous.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init
    , pages = TeamDataPage
    }


dumpModel : Model -> Cmd Msg
dumpModel model =
    Download.string
        (String.concat [ String.join "-" <| TeamData.getter model.teamData, ".txt" ])
        "content/text"
    <|
        String.join ","
            [ String.join "," <| TeamData.getter model.teamData
            , Autonomous.getter model.autonomousData
            , Teleop.getter model.teleopData
            , Climbing.getter model.climbingData
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamDataMsg teamMsg ->
            ( { model | teamData = TeamData.update teamMsg model.teamData }, Cmd.none )

        AutonomousDataMsg autoMsg ->
            ( { model | autonomousData = Autonomous.update autoMsg model.autonomousData }, Cmd.none )

        TeleopDataMsg telMsg ->
            ( { model | teleopData = Teleop.update telMsg model.teleopData }, Cmd.none )

        ClimbingDataMsg climbMsg ->
            ( { model | climbingData = Climbing.update climbMsg model.climbingData }, Cmd.none )

        PrevPage ->
            ( case model.pages of
                AutonomousPage ->
                    { model | pages = TeamDataPage }

                TeleopPage ->
                    { model | pages = AutonomousPage }

                ClimbingPage ->
                    { model | pages = TeleopPage }

                TeamDataPage ->
                    model
            , Cmd.none
            )

        NextPage ->
            let
                matchError : Result String GetMatch.Match
                matchError =
                    TeamData.getMatch model.teamData

                stationError : Result String Int
                stationError =
                    TeamData.getTeam2 model.teamData

                verifier : Bool
                verifier =
                    (not <| List.member matchError [ Err "No such match", Err "Invalid match number" ])
                        && stationError
                        /= Err "No station"
                        && (not << String.isEmpty << .scouterName << .teamData) model
                        || List.member model.teamData.scouterName [ "Itamar", "tom", "hadar", "shira" ]
            in
            ( if model.pages == TeamDataPage && verifier then
                { model | pages = AutonomousPage }

              else if model.pages == AutonomousPage then
                { model | pages = TeleopPage }

              else if model.pages == TeleopPage then
                { model | pages = ClimbingPage }

              else
                model
            , Cmd.none
            )

        Submit ->
            ( model, dumpModel model )


teamDataToString : Model -> String
teamDataToString model =
    model.teamData
        |> TeamData.getTeam2
        |> Result.map String.fromInt
        |> merge


view : Model -> Element.Element Msg
view model =
    let
        page : String -> PagePosition -> Element.Element Msg -> Element.Element Msg
        page name pagePosition msg =
            column
                [ centerX
                , width fill
                , Background.color << findColor <| TeamData.stationToString model.teamData.station
                , height fill
                ]
                [ column [ heightPrecent 7, width fill ]
                    [ text name
                        |> el
                            [ Font.size 50
                            , Font.color Colors.white
                            , Font.glow Colors.black 10
                            , centerX
                            , height <| fillPortion 1
                            , Font.underline
                            ]
                    , text ("scouted team: " ++ teamDataToString model)
                        |> el
                            [ Font.size 40
                            , Font.color Colors.white
                            , Font.glow Colors.black 10
                            , centerX
                            , height <| fillPortion 1
                            ]
                    ]
                , msg
                , stylishPage pagePosition
                ]
    in
    case model.pages of
        TeamDataPage ->
            model.teamData
                |> TeamData.view
                |> Element.map TeamDataMsg
                |> page "Registeration" FirstPage

        AutonomousPage ->
            Autonomous.view model.autonomousData
                |> Element.map AutonomousDataMsg
                |> page "Autonomous" MiddlePage

        TeleopPage ->
            Teleop.view model.teleopData
                |> Element.map TeleopDataMsg
                |> page "Teleop" MiddlePage

        ClimbingPage ->
            Climbing.view model.climbingData
                |> Element.map ClimbingDataMsg
                |> page "End-game" LastPage


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.color Colors.white
    , Font.size 60
    , Font.glow Colors.black 6
    , Border.rounded 10
    , Background.color Colors.gray
    , centerX
    , centerY
    , heightPrecent 70
    ]
