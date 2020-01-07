module Counter exposing (Model, Msg, init, update, view)

import Browser
import Colors exposing (black, blue, pink, red, sky, white, yellow)
import Element exposing (padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden)


type alias Model =
    { counter : Int }


type Msg
    = Plus
    | Minus


init : Int -> Model
init =
    Model


update : Int -> Int -> Msg -> Model -> Model
update max min msg model =
    case msg of
        Plus ->
            if model.counter == max then
                model

            else
                { model | counter = model.counter + 1 }

        Minus ->
            if model.counter == min then
                model

            else
                { model | counter = model.counter - 1 }


view : String -> Model -> Element.Element Msg
view title model =
    row
        [ padding 3
        , spacing 20
        ]
        [ text title
        , button [] { onPress = Just Minus, label = text "-" }
        , text <| String.fromInt model.counter
        , button [] { onPress = Just Plus, label = text "+" }
        ]
