module Counter exposing (Model, Msg, init, update, view)

import Element exposing (el, padding, row, spacing, text)
import Element.Font as Font
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
    let
        size : Element.Attribute Msg
        size =
            Font.size 70
    in
    row
        [ spacing 100 ]
        [ text title
        , row [ size, spacing 20 ]
            [ button [] { onPress = Just Minus, label = text "-" }
            , text <| String.fromInt model
            , button [] { onPress = Just Plus, label = text "+" }
            ]
        ]
