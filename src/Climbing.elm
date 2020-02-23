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


type Msg
    = TriedClimb
    | ClimbStatus Status
    | Balanced
    | Defended
    | WasDefended
    | ShutDown
    | Comment String


type alias Model =
    { triedClimb : Bool
    , climbStatus : Status
    , balanced : Bool
    , defended : Bool
    , wasDefended : Bool
    , shutDown : Bool
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
        , "shut down?" ++ "," ++ boolToString model.shutDown
        , "comments" ++ "," ++ "'" ++ model.comment ++ "'"
        ]


type Status
    = Hanged
    | Parked
    | Loser


init : Model
init =
    Model False Loser False False False False "" (TeamData.init <| Array.fromList GetMatch.matches)


update : Msg -> Model -> Model
update msg model =
    case msg of
        TriedClimb ->
            if model.climbStatus == Hanged then
                { model | triedClimb = True }

            else
                { model | triedClimb = not model.triedClimb }

        ClimbStatus status ->
            if model.balanced == True then
                { model | climbStatus = Hanged }

            else if status == Hanged then
                { model | climbStatus = status, triedClimb = True }

            else
                { model | climbStatus = status }

        Balanced ->
            if model.climbStatus == Loser || model.climbStatus == Parked then
                { model | balanced = False }

            else
                { model | balanced = not model.balanced }

        Defended ->
            { model | defended = not model.defended }

        WasDefended ->
            { model | wasDefended = not model.wasDefended }

        ShutDown ->
            { model | shutDown = not model.shutDown }

        Comment string ->
            let
                wordCounter =
                    modBy 20 (String.length string)
            in
            { model
                | comment =
                    if wordCounter == 0 then
                        string ++ "\n"

                    else
                        string
            }


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.multiline
        [ Font.color sky
        , Font.size 70
        , height fill
        ]
        { onChange = nextButton
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = labelHidden modelValue
        , spellcheck = True
        }


createButton : Msg -> String -> String -> Element.Element Msg
createButton msg title src =
    button
        [ width fill ]
        { onPress = Just msg
        , label =
            row
                [ spacing 50
                , Font.size 80
                , centerX
                ]
                [ text title
                , Element.image
                    [ height <| Element.maximum 80 fill
                    ]
                    { src = src, description = "" }
                ]
        }


decoration : List (Element.Attribute Msg)
decoration =
    [ spacing 100 ]


view : Model -> Element.Element Msg
view model =
    let
        buttonContent : Bool -> String
        buttonContent condition =
            if condition then
                "https://i.imgur.com/eiuQZig.png"

            else
                "https://i.imgur.com/SeSMGGI.png"
    in
    column
        [ Font.size 60
        , centerX
        , padding 70
        ]
        [ column decoration
            [ createButton TriedClimb "Tried hanging?" <| buttonContent model.triedClimb
            , column [ spacing 50, centerX ]
                [ el [ Font.underline, Font.size 80 ] (text "Climb status:")
                , el [ centerX ] <|
                    radio
                        [ spacing 50, Font.size 75 ]
                        { onChange = ClimbStatus
                        , selected = Just model.climbStatus
                        , label = Input.labelHidden "Climb status:"
                        , options =
                            [ Input.optionWith Loser <| forOptionWith "loser"
                            , Input.optionWith Parked <| forOptionWith "parked"
                            , Input.optionWith Hanged <| forOptionWith "hanged"
                            ]
                        }
                ]
            , column
                decoration
                [ createButton Balanced "Balanced?" <| buttonContent model.balanced
                , createButton Defended "Defended?" <| buttonContent model.defended
                , createButton WasDefended "Was defended?" <| buttonContent model.wasDefended
                , createButton ShutDown "Shut down?" <| buttonContent model.shutDown
                ]
            , textInput model.comment Comment "any comments?"
            ]
        ]


forOptionWith : String -> Input.OptionState -> Element.Element Msg
forOptionWith displayedText option =
    el
        (case option of
            Input.Idle ->
                [ Font.color Colors.gray, centerX ]

            Input.Focused ->
                [ Font.color black ]

            Input.Selected ->
                [ Font.bold, centerX ]
        )
    <|
        text displayedText
