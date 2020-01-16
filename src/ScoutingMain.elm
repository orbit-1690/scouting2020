module ScoutingMain exposing (Model, Msg, init, subscriptions, update, view)

import Autonomous
import Browser
import Climbing
import Colors exposing (black, blue, blueGreen, lightBlue, orange, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, fill, height, layout, maximum, padding, rgb255, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input exposing (button)
import GetMatch exposing (getMatch, maybeIntToInt, unwrapToString)
import Http
import Maybe
import TeamData
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout []
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = subscriptions
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
    | NextPage


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    }


stylishPage : Element.Element Msg -> Element.Element Msg
stylishPage page =
    column
        [ Background.color lightBlue
        , padding 10
        , spacing 10
        , width fill
        , height fill
        ]
        [ page
        , button
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
            , Background.gradient
                { angle = 2
                , steps = [ purple, orange, blueGreen ]
                }
            , center
            , centerX
            , centerY
            , width <| maximum 350 <| fill
            ]
            { onPress = Just <| NextPage
            , label = Element.text "Next Page"
            }
        ]


init : Model
init =
    { teamData = TeamData.init "" Nothing Nothing
    , autonomousData = Autonomous.init Autonomous.Stage1 False Counter.init Counter.init Counter.init <| Counter.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init 0
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

        NextPage ->
            if model.pages == AutonomousPage then
                { model | pages = TeleopPage }

            else
                { model | pages = AutonomousPage }


view : Model -> Element.Element Msg
view model =
    case model.pages of
        TeamDataPage ->
            stylishPage << Element.map TeamDataMsg <| TeamData.teamDataView model.teamData

        AutonomousPage ->
            stylishPage << Element.map AutonomousDataMsg <| Autonomous.autonomousView model.autonomousData

        TeleopPage ->
            stylishPage << Element.map TeleopDataMsg <| Teleop.teleopView model.teleopData

        ClimbingPage ->
            stylishPage << Element.map ClimbingDataMsg <| Climbing.view model.climbingData


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Sub.map AutonomousDataMsg <| Autonomous.subscriptions
        , Sub.map TeamDataMsg <| TeamData.subscriptions
        , Sub.map TeleopDataMsg <| Teleop.subscriptions
        ]
