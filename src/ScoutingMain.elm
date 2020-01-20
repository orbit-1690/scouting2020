module ScoutingMain exposing (Model, Msg, init, subscriptions, update, view)

import Autonomous
import Browser
import Climbing exposing (Status)
import Colors exposing (black, blue, blueGreen, lightBlue, orange, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, fill, height, layout, maximum, padding, rgb255, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button)
import GetMatch exposing (getMatch, maybeIntToInt, stationIndex, unwrapToString)
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
            rainbowStyle
            { onPress = Just <| NextPage
            , label = Element.text "Next Page"
            }
        , button
            rainbowStyle
            { onPress = Just <| PrevPage
            , label = Element.text "Previous Page"
            }
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
            if model.pages == AutonomousPage then
                { model | pages = TeamDataPage }

            else if model.pages == TeleopPage then
                { model | pages = AutonomousPage }

            else if model.pages == ClimbingPage then
                { model | pages = TeleopPage }

            else
                model

        NextPage ->
            let
                error =
                    getMatch model.teamData.match model.teamData.station
            in
            if model.pages == TeamDataPage && (error /= "Not a match") && (error /= "Team not in this match") then
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
            stylishPage <| Element.map TeamDataMsg <| TeamData.teamDataView model.teamData

        AutonomousPage ->
            stylishPage <| Element.map AutonomousDataMsg <| Autonomous.autonomousView model.autonomousData

        TeleopPage ->
            stylishPage <| Element.map TeleopDataMsg <| Teleop.teleopView model.teleopData

        ClimbingPage ->
            stylishPage <| Element.map ClimbingDataMsg <| Climbing.view model.climbingData


subscriptions : Sub Msg
subscriptions =
    Sub.batch
        [ Sub.map AutonomousDataMsg <| Autonomous.subscriptions
        , Sub.map TeamDataMsg <| TeamData.subscriptions
        , Sub.map TeleopDataMsg <| Teleop.subscriptions
        ]


rainbowStyle : List (Element.Attribute Msg)
rainbowStyle =
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
