module Teleop exposing (Model, Msg, init, subscriptions, update, view)

import Colors exposing (black, blue, blueGreen, lightBlue, orange, purple, sky, white)
import Element exposing (centerX, centerY, column, fill, height, layout, maximum, padding, rgb255, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden)
import Http
import Maybe
import String


type Msg
    = Todo


type alias Model =
    { todo : Int }


teleopView : Model -> Element.Element Msg
teleopView model =
    Debug.todo "view"


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.text
        [ Font.color sky
        , Font.size 20
        , height fill
        , Font.family
            [ Font.external
                { name = "Pacifico"
                , url = "https://fonts.googleapis.com/css?family=Pacifico"
                }
            ]
        ]
        { onChange = nextButton
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = labelHidden modelValue
        }


init : Int -> Model
init =
    Model


update : Msg -> Model -> Model
update msg model =
    Debug.todo "update"


view : Model -> Element.Element Msg
view model =
    teleopView model


subscriptions : Sub Msg
subscriptions =
    Sub.none
