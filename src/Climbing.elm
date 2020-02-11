module Climbing exposing (Model, Msg, getter, init, subscriptions, update, view)

import Array
import Colors exposing (black, blue, purple, sky, white)
import Element exposing (centerX, centerY, column, el, fill, height, padding, row, spacing, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden, radioRow)
import GetMatch
import TeamData
import Teleop exposing (boolToText)


type Msg
    = TriedClimb
    | ClimbStatus Status
    | Balanced
    | Defended
    | WasDefended
    | Comment String


type alias Model =
    { triedClimb : Bool
    , climbStatus : Status
    , balanced : Bool
    , defended : Bool
    , wasDefended : Bool
    , comment : String
    , localTeam : TeamData.Model
    }


getter : Model -> String
getter model =
    let
        boolToString : Bool -> String
        boolToString bool =
            if bool then
                "true"

            else
                "false"

        statusToString : Status -> String
        statusToString status =
            case status of
                Hanged ->
                    "Hanged"

                Parked ->
                    "Parked"

                Loser ->
                    "Loser"
    in
    String.join ","
        [ boolToString model.triedClimb
        , boolToString model.balanced
        , statusToString model.climbStatus
        , boolToString model.defended
        , boolToString model.wasDefended
        , "'" ++ model.comment ++ "'"
        ]


type Status
    = Hanged
    | Parked
    | Loser


init : Model
init =
    Model False Loser False False False "" (TeamData.init <| Array.fromList GetMatch.matches)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TriedClimb ->
            { model | triedClimb = not model.triedClimb }

        ClimbStatus status ->
            { model | climbStatus = status }

        Balanced ->
            { model | balanced = not model.balanced }

        Defended ->
            { model | defended = not model.defended }

        WasDefended ->
            { model | wasDefended = not model.wasDefended }

        Comment string ->
            { model | comment = string }


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.text
        [ Font.color sky
        , Font.size 60
        , rounded 10
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
        , Font.size 60
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
    , Font.size 60
    , centerX
    , centerY
    ]


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color sky
        , padding 50
        , spacing 20
        , rounded 20
        , centerX
        , centerY
        ]
        [ column yophyTophy
            [ row
                yophyTophy
                [ column yophyTophy
                    [ el yophyTophy
                        (text "Tried hanging?")
                    , createButton TriedClimb <| boolToText model.triedClimb
                    ]
                , column yophyTophy
                    [ el yophyTophy
                        (text "Balanced?")
                    , createButton Balanced <| boolToText model.balanced
                    ]
                ]
            , radio
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
            , row yophyTophy
                [ column yophyTophy
                    [ el yophyTophy
                        (text "Defended?")
                    , createButton Defended <| boolToText model.defended
                    ]
                , column yophyTophy
                    [ el yophyTophy
                        (text "Was defended?")
                    , createButton WasDefended <| boolToText model.wasDefended
                    ]
                ]
            , textInput model.comment Comment "any comments?"
            ]
        ]


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


subscriptions : Sub Msg
subscriptions =
    Sub.none
