module Counter exposing (Model, Msg, init, update, view)

import Element exposing (alignRight, centerX, centerY, fill, px, row, spacing, text, width)
import Element.Font exposing (center, size)
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
        buttonElements : List (Element.Attribute Msg)
        buttonElements =
            [ size 110
            , width <| px 100
            , center
            ]
    in
    row
        [ width fill ]
        [ text title
        , row [ spacing 10, alignRight ]
            [ button buttonElements { onPress = Just Minus, label = Element.el [ centerX, centerY, Element.moveUp 9.8 ] <| text "-" }
            , text <| String.fromInt model
            , button buttonElements { onPress = Just Plus, label = text "+" }
            ]
        ]
