module Counter exposing (Model, Msg, init, update, view)

import Colors
import Element exposing (alignLeft, alignRight, el, padding, row, spacing, text)
import Element.Background exposing (color)
import Element.Font as Font exposing (size)
import Element.Input exposing (button)


type alias Model =
    Int


type Msg
    = Plus
    | Minus


init : Model
init =
    0


update : Msg -> Model -> Model
update msg model =
    case msg of
        Plus ->
            model + 1

        Minus ->
            max 0 <| model - 1


view : String -> Model -> Element.Element Msg
view title model =
    row
        [ Element.width Element.fill ]
        [ text title
        , row [ size 60, spacing 20, alignRight ]
            [ button [ size 90 ] { onPress = Just Minus, label = text "-" }
            , text <| String.fromInt model
            , button [ size 90 ] { onPress = Just Plus, label = text "+" }
            ]
        ]
