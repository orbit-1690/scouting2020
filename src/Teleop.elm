module Teleop exposing (Model, Msg, init, subscriptions, teleopView, update)

import Colors exposing (black, blue, blueGreen, lightBlue, orange, pink, purple, sky, white, yellow)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, minimum, padding, px, rgb, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden)
import Http
import Maybe


type Msg
    = LevelOne Counter.Msg
    | LevelTwo Counter.Msg
    | LevelThree Counter.Msg
    | FloorCollecting
    | FiderCollecting
    | ColorRoulette
    | SpinedRoulette


type alias Model =
    { levelOne : Counter.Model
    , levelTwo : Counter.Model
    , levelThree : Counter.Model
    , floorCollecting : Bool
    , fiderCollecting : Bool
    , colorRoulette : Bool
    , spinedRoulette : Bool
    }


init : Model
init =
    Model (Counter.Model 0) (Counter.Model 0) (Counter.Model 0) False False False False


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        LevelOne count1 ->
            { model | levelOne = Counter.update 15 0 count1 model.levelOne }

        LevelTwo count2 ->
            { model | levelTwo = Counter.update 15 0 count2 model.levelTwo }

        LevelThree count3 ->
            { model | levelThree = Counter.update 15 0 count3 model.levelThree }

        FloorCollecting ->
            { model | floorCollecting = not model.floorCollecting }

        FiderCollecting ->
            { model | fiderCollecting = not model.fiderCollecting }

        ColorRoulette ->
            { model | colorRoulette = not model.colorRoulette }

        SpinedRoulette ->
            { model | spinedRoulette = not model.spinedRoulette }


teleopView : Model -> Element.Element Msg
teleopView model =
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
        [ row yophyTophy
            [ column yophyTophy
                [ createButton FloorCollecting "floor colected?"
                , printButton "didn't floor collected" "floor collected" model.floorCollecting
                ]
            , column
                yophyTophy
                [ createButton FiderCollecting "fider colected?"
                , printButton "didn't fider collected" "fider collected" model.fiderCollecting
                ]
            ]
        , row yophyTophy
            [ column yophyTophy
                [ createButton ColorRoulette "grererreererere?"
                , printButton "banana" "ferero" model.colorRoulette
                ]
            , column
                yophyTophy
                [ createButton SpinedRoulette "blablablab?"
                , printButton "ys" "n" model.spinedRoulette
                ]
            ]
        , Element.map LevelOne <| Counter.view "Level 1:" model.levelOne
        , Element.map LevelTwo <| Counter.view "Level 2:" model.levelTwo
        , Element.map LevelThree <| Counter.view "Level 3:" model.levelThree
        ]


subscriptions : Sub Msg
subscriptions =
    Sub.none


yophyTophy : List (Element.Attribute Msg)
yophyTophy =
    [ padding 10
    , spacing 5
    , centerX
    , centerY
    ]
