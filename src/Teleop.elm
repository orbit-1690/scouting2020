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
    = LowLevel Counter.Msg
    | HighLevel Counter.Msg
    | Missed Counter.Msg
    | FloorCollecting
    | FiderCollecting
    | ColorRoulette
    | SpinedRoulette


type alias Model =
    { lowlevel : Counter.Model
    , highlevel : Counter.Model
    , missed : Counter.Model
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
        LowLevel count ->
            { model | lowlevel = Counter.update 15 0 count model.lowlevel }

        HighLevel count ->
            { model | highlevel = Counter.update 15 0 count model.highlevel }

        FloorCollecting ->
            { model | floorCollecting = not model.floorCollecting }

        FiderCollecting ->
            { model | fiderCollecting = not model.fiderCollecting }

        ColorRoulette ->
            { model | colorRoulette = not model.colorRoulette }

        SpinedRoulette ->
            { model | spinedRoulette = not model.spinedRoulette }

        Missed count ->
            { model | missed = Counter.update 15 0 count model.missed }


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
                [ createButton ColorRoulette "correct color?"
                , printButton "no" "yes" model.colorRoulette
                ]
            , column
                yophyTophy
                [ createButton SpinedRoulette "spun roulette?"
                , printButton "no" "yes" model.spinedRoulette
                ]
            ]
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map HighLevel <| Counter.view "high Level:" model.highlevel
        , Element.map Missed <| Counter.view "missed:" model.missed
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
