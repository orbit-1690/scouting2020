module Teleop exposing (Model, Msg, createButton, decoration, getter, init, printButton, update, view)

import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, fill, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input exposing (button)


type Msg
    = LowLevel Counter.Msg
    | LevelTwo Counter.Msg
    | LevelThree Counter.Msg
    | Missed Counter.Msg
    | ColorRoulette
    | SpunRoulette


type alias Model =
    { lowlevel : Counter.Model
    , levelTwo : Counter.Model
    , levelThree : Counter.Model
    , missed : Counter.Model
    , colorRoulette : Bool
    , spunRoulette : Bool
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
    in
    String.join "\n"
        [ "colorRoulette" ++ "," ++ boolToString model.colorRoulette
        , "spunRoulette?" ++ "," ++ boolToString model.spunRoulette
        , "level 1." ++ "," ++ String.fromInt model.lowlevel
        , "level 2." ++ "," ++ String.fromInt model.levelTwo
        , "level 3." ++ "," ++ String.fromInt model.levelThree
        , "missed." ++ "," ++ String.fromInt model.missed
        ]


init : Model
init =
    Model Counter.init Counter.init Counter.init Counter.init False False


createButton : Msg -> String -> Element.Element Msg
createButton msg name =
    button
        [ Font.color white
        , Font.size 25
        , Font.glow blue 5
        , rounded 10
        , Font.bold
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
                }
            ]
        , Background.color purple
        , center
        , centerX
        , centerY
        ]
        { onPress = Just msg, label = text name }


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


update : Msg -> Model -> Model
update msg model =
    let
        counterUpdate : Counter.Msg -> Counter.Model -> Counter.Model
        counterUpdate =
            Counter.update
    in
    case msg of
        LowLevel count ->
            { model | lowlevel = counterUpdate count model.lowlevel }

        LevelTwo count ->
            { model | levelTwo = counterUpdate count model.levelTwo }

        LevelThree count ->
            { model | levelThree = counterUpdate count model.levelThree }

        ColorRoulette ->
            { model | colorRoulette = not model.colorRoulette }

        SpunRoulette ->
            { model | spunRoulette = not model.spunRoulette }

        Missed count ->
            { model | missed = counterUpdate count model.missed }


view : Model -> Element.Element Msg
view model =
    column
        [ Background.color blue
        , padding 50
        , spacing 70
        , centerX
        , Element.height <| Element.fillPortion 3
        , centerY
        , width fill
        ]
        [ row decoration
            [ column decoration
                [ createButton SpunRoulette "spun cycles 3-5?"
                , printButton "no" "yes" model.spunRoulette
                ]
            , column decoration
                [ createButton ColorRoulette "spun to\ncorrect color?"
                , printButton "no" "yes" model.colorRoulette
                ]
            ]
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map LevelTwo <| Counter.view "second Level:" model.levelTwo
        , Element.map LevelThree <| Counter.view "third Level:" model.levelThree
        , Element.map Missed <| Counter.view "missed:" model.missed
        ]


decoration : List (Element.Attribute Msg)
decoration =
    [ padding 10
    , spacing 5
    , centerX
    , centerY
    ]
