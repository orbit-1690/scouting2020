module Climbing exposing (Model, Msg, init, subscriptions, update, view)

import Colors exposing (black, blue, purple, sky, white)
import Element exposing (centerX, centerY, column, el, fill, height, padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden, radioRow)


type Msg
    = TriedClimb
    | ClimbStatus State
    | Balanced
    | Defended
    | WasDefended
    | Comment String


type alias Model =
    { triedClimb : Bool
    , climbStatus : State
    , balanced : Bool
    , defended : Bool
    , wasDefended : Bool
    , comment : String
    , postStatus : PostStatus
    }


init : Model
init =
    Model False Loser False False False "" NoStatus


type State
    = Hanged
    | Parked
    | Loser


type PostStatus
    = NoStatus
    | Sent
    | Received


view : Model -> Element.Element Msg
view model =
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
        [ column yophyTophy
            [ row
                yophyTophy
                [ column yophyTophy
                    [ createButton TriedClimb "Tried hanging?"
                    , printButton "no" "yes" model.triedClimb
                    ]
                , column yophyTophy
                    [ createButton Balanced "Balanced?"
                    , printButton "no" "yes" model.balanced
                    ]
                ]
            , radioRow
                [ padding 10
                , spacing 20
                ]
                { onChange = ClimbStatus
                , selected = Just model.climbStatus
                , label = Input.labelAbove [] (text " climb status:")
                , options =
                    [ Input.option Loser (text "loser")
                    , Input.option Parked (text "parked")
                    , Input.option Hanged (text "hanged")
                    ]
                }
            , row
                yophyTophy
                [ column yophyTophy
                    [ createButton Defended "Defended?"
                    , printButton "no" "yes" model.defended
                    ]
                , column yophyTophy
                    [ createButton WasDefended "Was defended?"
                    , printButton "no" "yes" model.wasDefended
                    ]
                ]
            , textInput model.comment Comment "any comments?"
            ]
        ]


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
        , Background.color purple
        , center
        , centerX
        , centerY
        ]
        { onPress = Just msg, label = text name }


yophyTophy : List (Element.Attribute Msg)
yophyTophy =
    [ padding 10
    , spacing 5
    , centerX
    , centerY
    ]


update : Msg -> Model -> Model
update msg model =
    case msg of
        TriedClimb ->
            { model | triedClimb = not model.triedClimb }

        ClimbStatus state ->
            { model | climbStatus = state }

        Balanced ->
            { model | balanced = not model.balanced }

        Defended ->
            { model | defended = not model.defended }

        WasDefended ->
            { model | wasDefended = not model.wasDefended }

        Comment string ->
            { model | comment = string }


subscriptions : Sub Msg
subscriptions =
    Sub.none


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
