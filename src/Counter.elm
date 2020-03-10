module Counter exposing (Model, Msg, init, update, view)

import Element exposing (alignRight, centerX, centerY, fill, image, px, row, spacing, text, width)
import Element.Font as Font exposing (center, size)
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


view : Element.Element Msg -> Model -> Element.Element Msg
view title model =
    let
        buttonElements : List (Element.Attribute Msg)
        buttonElements =
            [ size 110
            , width <| px 100
            , center
            ]

        buttonImage : String -> Element.Element Msg
        buttonImage src =
            image
                [ Element.height <| Element.maximum 90 fill ]
                { src = src, description = "" }
    in
    row
        [ width fill ]
        [ title
        , row [ spacing 10, alignRight, Font.size 80 ]
            [ button buttonElements { onPress = Just Minus, label = buttonImage "https://i.imgur.com/gWiof6q.png" }
            , text <| String.fromInt model
            , button buttonElements { onPress = Just Plus, label = buttonImage "https://i.imgur.com/BMg1XjN.png" }
            ]
        ]
