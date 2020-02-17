module Climbing exposing (Model, Msg, getter, init, update, view)

import Array
import Colors exposing (black, blue, purple, sky, white)
import Element exposing (centerX, centerY, column, el, fill, height, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden, radio)
import GetMatch
import TeamData
import Teleop


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
                "1"

            else
                "0"

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
    String.join "\n"
        [ "triedClimb" ++ "," ++ boolToString model.triedClimb
        , "balanced?" ++ "," ++ boolToString model.balanced
        , "final state" ++ "," ++ statusToString model.climbStatus
        , "defended?" ++ "," ++ boolToString model.defended
        , "was defended?" ++ "," ++ boolToString model.wasDefended
        , "comments" ++ "," ++ "'" ++ model.comment ++ "'"
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
            if model.balanced == True then
                { model | climbStatus = Hanged }

            else
                { model | climbStatus = status }

        Balanced ->
            if model.climbStatus == Loser then
                { model | balanced = False }

            else
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
        , Font.size 20
        , height fill
        , Font.family
            [ Font.external
                { name = "open-sans"
                , url = "https://fonts.googleapis.com/css?family=open-sans"
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
        , Font.bold
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


decoration : List (Element.Attribute Msg)
decoration =
    [ padding 10
    , spacing 5
    , centerX
    , centerY
    ]


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color blue
        , padding 50
        , height <| Element.fillPortion 5
        , centerX
        , centerY
        , width fill
        ]
        [ column decoration
            [ row
                decoration
                [ column decoration
                    [ createButton TriedClimb "Tried hanging?"
                    , printButton "no" "yes" model.triedClimb
                    ]
                , column decoration
                    [ createButton Balanced "Balanced?"
                    , printButton "no" "yes" model.balanced
                    ]
                ]
            , radio
                [ padding 10
                , spacing 70
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
                decoration
                [ column decoration
                    [ createButton Defended "Defended?"
                    , printButton "no" "yes" model.defended
                    ]
                , column decoration
                    [ createButton WasDefended "Was defended?"
                    , printButton "no" "yes" model.wasDefended
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
