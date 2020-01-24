module Autonomous exposing (Model, Msg, Stages(..), autonomousView, init, subscriptions, update)

import Colors exposing (black, blue, blueGreen, lightBlue, orange, pink, purple, sky, white, yellow)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, minimum, padding, px, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden, radioRow)


type Msg
    = Moved
    | LevelOne Counter.Msg
    | LevelTwo Counter.Msg
    | LevelThree Counter.Msg
    | Collection Counter.Msg
    | Stage Stages


type alias Model =
    { stage : Stages
    , moved : Bool
    , levelOne : Counter.Model
    , levelTwo : Counter.Model
    , levelThree : Counter.Model
    , collection : Counter.Model
    }


type Stages
    = Stage1
    | Stage2
    | Stage3


createButton : Msg -> String -> Element.Element Msg
createButton msg name =
    button
        [ Font.color white
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
        { onPress = Just msg, label = text name }


printButton : String -> String -> Bool -> Element.Element Msg
printButton onFalse onTrue modelBool =
    el
        [ center
        , centerX
        , centerY
        ]
        (text <|
            if modelBool then
                onTrue

            else
                onFalse
        )


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
        [ radioRow
            [ padding 10
            , spacing 20
            ]
            { onChange = Stage
            , selected = Just model.stage
            , label = Input.labelAbove [] (text "Stages")
            , options =
                [ Input.option Stage1 (text "Stage1")
                , Input.option Stage2 (text "Stage2")
                , Input.option Stage3 (text "Stage3")
                ]
            }
        , createButton Moved "moved?"
        , printButton "didn't move" "moved" model.moved
        , Element.map LevelOne <| Counter.view "Level 1:" model.levelOne
        , Element.map LevelTwo <| Counter.view "Level 2:" model.levelTwo
        , Element.map LevelThree <| Counter.view "Level 3:" model.levelThree
        , Element.map Collection <| Counter.view "Collected:" model.collection
        ]


init : Stages -> Bool -> Counter.Model -> Counter.Model -> Counter.Model -> Counter.Model -> Model
init =
    Model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LevelOne count1 ->
            { model | levelOne = Counter.update 15 0 count1 model.levelOne }

        LevelTwo count2 ->
            { model | levelTwo = Counter.update 15 0 count2 model.levelTwo }

        LevelThree count3 ->
            { model | levelThree = Counter.update 15 0 count3 model.levelThree }

        Collection count ->
            { model | collection = Counter.update 15 0 count model.collection }

        Stage stage ->
            { model | stage = stage }


subscriptions : Sub Msg
subscriptions =
    Sub.none
