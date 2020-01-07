module Autonomous exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (toString)
import Colors exposing (black, blue, blueGreen, lightBlue, orange, pink, purple, sky, white, yellow)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, minimum, padding, px, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden)
import Http
import Maybe
import String


type Msg
    = Moved
    | LevelOne Counter.Msg
    | LevelTow Counter.Msg
    | LevelThree Counter.Msg


type alias Model =
    { moved : Bool
    , levelOne : Counter.Model
    , levelTow : Counter.Model
    , levelThree : Counter.Model
    }


autonomousView : Model -> Element.Element Msg
autonomousView model =
    column
        [ Background.color sky
        , Border.color black
        , padding 50
        , spacing 20
        , widthXY 5 5
        , rounded 10
        , centerX
        , centerY
        ]
        [ button
            [ Font.color
                white
            , Font.size 25
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
                , steps = [ pink, yellow, white ]
                }
            , center
            , centerX
            , centerY
            ]
            { onPress = Just Moved, label = text "moved?" }
        , el
            [ center
            , centerX
            , centerY
            ]
            (text <| toString model.moved)
        , Element.map LevelOne <| Counter.view "Level 1:" model.levelOne
        , Element.map LevelTow <| Counter.view "Level 2:" model.levelTow
        , Element.map LevelThree <| Counter.view "Level 3:" model.levelThree
        ]


init : Bool -> Counter.Model -> Counter.Model -> Counter.Model -> Model
init =
    Model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LevelOne count1 ->
            { model | levelOne = Counter.update 9 0 count1 model.levelOne }

        LevelTow count2 ->
            { model | levelTow = Counter.update 9 0 count2 model.levelTow }

        LevelThree count3 ->
            { model | levelThree = Counter.update 9 0 count3 model.levelThree }


view : Model -> Element.Element Msg
view model =
    autonomousView model


subscriptions : Sub Msg
subscriptions =
    Sub.none
