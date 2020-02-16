module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous
import Browser
import Browser.Events as BE
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (Color, Device, centerX, centerY, column, el, fill, height, htmlAttribute, layout, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
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
        , view = view >> layout [ width fill, htmlAttribute <| style "touch-action" "manipulation" ]
        , update = update
        , subscriptions = always Sub.none
        }


type Pages
    = TeamDataPage
    | AutonomousPage
    | TeleopPage
    | ClimbingPage
    | SubmitPage


type Msg
    = TeamDataMsg TeamData.Msg
    | AutonomousDataMsg Autonomous.Msg
    | TeleopDataMsg Teleop.Msg
    | ClimbingDataMsg Climbing.Msg
    | SubmitDataMsg Msg
    | ScreenSize Device
    | PrevPage
    | NextPage
    | Submit
    | YesSubmit
    | NoSubmit


type PagePosition
    = FirstPage
    | MiddlePage
    | LastPage
    | SubmitPosPage


type alias DeviceSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    , screenSize : Device
    }


type BackGroundColorOptions
    = Blue Color
    | Red Color


findColor : String -> Element.Color
findColor alliance =
    if String.contains "Blue" alliance then
        Colors.blue

    else if String.contains "Red" alliance then
        Colors.red

    else
        Colors.yellow


stylishPage : String -> PagePosition -> String -> String -> Element.Element Msg -> Element.Element Msg
stylishPage station position title teamNumber page =
    let
        decoration : Int -> List (Element.Attribute Msg)
        decoration size =
            [ padding 10
            , spacing 10
            , centerX
            , centerY
            , Font.color Colors.veryLightBlue
            , Font.size size
            ]

        createButtons : Msg -> String -> Msg -> String -> Element.Element Msg
        createButtons b1Msg title1Msg b2Msg title2Msg =
            column
                [ spacing 15, centerX, centerY ]
                [ button
                    buttonStyle
                    { onPress = Just <| b1Msg
                    , label = Element.text title1Msg
                    }
                , button
                    buttonStyle
                    { onPress = Just <| b2Msg
                    , label = Element.text title2Msg
                    }
                ]
    in
    column
        [ Background.color <| findColor station
        , spacing 15
        , width fill
        , height fill
        ]
        [ el
            (decoration 20)
            (text <| title)
        , el
            (decoration 15)
            (text <| "\nscouted team: " ++ teamNumber)
        , page
        , case position of
            FirstPage ->
                button
                    buttonStyle
                    { onPress = Just <| NextPage
                    , label = Element.text "Next Page"
                    }

            LastPage ->
                createButtons PrevPage "Previous Page" Submit "Submit"

            MiddlePage ->
                createButtons NextPage "Next Page" PrevPage "Previous Page"

            SubmitPosPage ->
                createButtons YesSubmit "Yes" NoSubmit "No"
        ]


init : Model
init =
    { teamData = TeamData.init <| Array.fromList GetMatch.matches
    , autonomousData = Autonomous.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init
    , pages = TeamDataPage
    , screenSize = Element.classifyDevice <| DeviceSize 0 0
    }


dumpModel : Model -> Cmd Msg
dumpModel model =
    Download.string
        (String.concat [ String.join "-" <| TeamData.getter model.teamData, ".csv" ])
        "content/text"
    <|
        String.join "\n"
            [ String.join "\n" <| TeamData.getter model.teamData
            , Autonomous.getter model.autonomousData
            , Teleop.getter model.teleopData
            , Climbing.getter model.climbingData
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ScreenSize device ->
            ( { model | screenSize = device }, Cmd.none )

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

                SubmitPage ->
                    model

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
                    (not <| List.member matchError [ Err "No such match", Err "Match number must be a number" ])
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

        YesSubmit ->
            ( model, dumpModel model )

        NoSubmit ->
            ( { model | pages = ClimbingPage }, Cmd.none )

        Submit ->
            ( { model | pages = SubmitPage }, Cmd.none )

        SubmitDataMsg _ ->
            ( model, Cmd.none )


view : Model -> Element.Element Msg
view model =
    let
        page : String -> PagePosition -> Element.Element Msg -> Element.Element Msg
        page name pagePosition =
            el
                [ Background.color <| findColor (TeamData.stationToString model.teamData.station)
                , padding 105
                , spacing 10
                , width fill
                , height fill
                , centerY
                , centerX
                ]
                << stylishPage
                    (TeamData.stationToString model.teamData.station)
                    pagePosition
                    name
                    (TeamData.getTeam2 model.teamData
                        |> Result.map String.fromInt
                        |> merge
                    )
    in
    case model.pages of
        TeamDataPage ->
            page
                "Registeration"
                FirstPage
                << Element.map TeamDataMsg
            <|
                TeamData.view model.teamData

        AutonomousPage ->
            page
                "Autonomous"
                MiddlePage
                << Element.map AutonomousDataMsg
            <|
                Autonomous.view model.autonomousData

        TeleopPage ->
            page
                "Teleop"
                MiddlePage
                << Element.map TeleopDataMsg
            <|
                Teleop.view model.teleopData

        ClimbingPage ->
            page
                "End-game"
                LastPage
                << Element.map ClimbingDataMsg
            <|
                Climbing.view model.climbingData

        SubmitPage ->
            page
                "Submit"
                SubmitPosPage
                << Element.map SubmitDataMsg
            <|
                column [ centerY, centerX ]
                    [ el [ Font.size 70, centerX, centerY ]
                        (text "Are you sure")
                    , el [ Font.size 70, centerX, centerY ]
                        (text "you want to submit?")
                    ]


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.color white
    , Font.size 60
    , Font.glow blue 5
    , Border.rounded 10
    , Font.family
        [ Font.external
            { name = "Open Sans"
            , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
            }
        ]
    , Background.color purple
    , center
    , centerX
    , centerY
    ]
