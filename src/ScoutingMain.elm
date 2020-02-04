module ScoutingMain exposing (Model, Msg, init, subscriptions, update, view)

import Autonomous
import Browser
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, layout, maximum, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import GetMatch exposing (getMatch)
import Html.Attributes exposing (style)
import TeamData exposing (nameCheck)
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout [ htmlAttribute <| style "touch-action" "manipulation" ]
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = \model -> subscriptions
        }


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


stylishPage : PagePosition -> String -> String -> Element.Element Msg -> Element.Element Msg
stylishPage position title teamNumber page =
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
    in
    column
        [ Background.color blue
        , padding 105
        , spacing 10
        , width fill
        , height fill
        , centerY
        ]
        [ el
            (decoration 50)
            (text <| title)
        , el
            (decoration 35)
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
                button
                    buttonStyle
                    { onPress = Just <| PrevPage
                    , label = Element.text "Previous Page"
                    }

            MiddlePage ->
                column
                    [ Background.color blue
                    , spacing 10
                    , width fill
                    , height fill
                    ]
                    [ button
                        buttonStyle
                        { onPress = Just <| NextPage
                        , label = Element.text "Next Page"
                        }
                    , button
                        buttonStyle
                        { onPress = Just <| PrevPage
                        , label = Element.text "Previous Page"
                        }
                    ]
        ]


init : Model
init =
    { teamData = TeamData.init
    , autonomousData = Autonomous.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init
    , pages = TeamDataPage
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        TeamDataMsg teamMsg ->
            { model | teamData = TeamData.update teamMsg model.teamData }

        AutonomousDataMsg autoMsg ->
            { model | autonomousData = Autonomous.update autoMsg model.autonomousData }

        TeleopDataMsg telMsg ->
            { model | teleopData = Teleop.update telMsg model.teleopData }

        ClimbingDataMsg climbMsg ->
            { model | climbingData = Climbing.update climbMsg model.climbingData }

        PrevPage ->
            case model.pages of
                AutonomousPage ->
                    { model | pages = TeamDataPage }

                TeleopPage ->
                    { model | pages = AutonomousPage }

                ClimbingPage ->
                    { model | pages = TeleopPage }

                TeamDataPage ->
                    model

        NextPage ->
            let
                error : String
                error =
                    getMatch model.teamData.match <| TeamData.stationToString model.teamData.station

                verifier : Bool
                verifier =
                    (error /= "Not a match") && (error /= "Team not in this match") && nameCheck model.teamData || model.teamData.scouterName == "Itamar" || model.teamData.scouterName == "tom"
            in
            if model.pages == TeamDataPage && verifier then
                { model | pages = AutonomousPage }

            else if model.pages == AutonomousPage then
                { model | pages = TeleopPage }

            else if model.pages == TeleopPage then
                { model | pages = ClimbingPage }

            else
                model


view : Model -> Element.Element Msg
view model =
    case model.pages of
        TeamDataPage ->
            stylishPage FirstPage "Registeration" (TeamData.team model.teamData) <| Element.map TeamDataMsg <| TeamData.view model.teamData

        AutonomousPage ->
            stylishPage MiddlePage "Autonomous" (TeamData.team model.teamData) <| Element.map AutonomousDataMsg <| Autonomous.view model.autonomousData

        TeleopPage ->
            stylishPage MiddlePage "Teleop" (TeamData.team model.teamData) <| Element.map TeleopDataMsg <| Teleop.view model.teleopData

        ClimbingPage ->
            stylishPage LastPage "End-game" (TeamData.team model.teamData) <| Element.map ClimbingDataMsg <| Climbing.view model.climbingData


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Sub.map AutonomousDataMsg <| Autonomous.subscriptions
        , Sub.map TeamDataMsg <| TeamData.subscriptions
        , Sub.map TeleopDataMsg <| Teleop.subscriptions
        , Sub.map ClimbingDataMsg <| Climbing.subscriptions
        ]


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.color white
    , Font.size 40
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
    , width <| maximum 350 <| fill
    ]
