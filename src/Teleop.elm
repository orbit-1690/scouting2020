module Teleop exposing (Model, Msg, createButton, decoration, getter, init, update, view)

import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, image, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button)
import Html.Attributes exposing (style)


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


createButton : Msg -> String -> String -> Element.Element Msg
createButton msg title src =
    row
        [ spacing 50
        , Font.size 60
        ]
        [ text title
        , button
            [ Font.color white
            , Font.size 60
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
            { onPress = Just msg
            , label =
                image
                    [ Font.size 20
                    , width <| Element.maximum 100 fill
                    ]
                    { src = src, description = "" }
            }
        ]


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
    let
        buttonState : Bool -> String
        buttonState state =
            if state then
                "https://i.imgur.com/9SXgxID.png"

            else
                "https://i.imgur.com/9eZzWtk.png"
    in
    column
        [ Background.color blue
        , padding 50
        , spacing 100
        , centerX
        , centerY
        , height fill
        , width fill
        ]
        [ Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map LevelTwo <| Counter.view "second Level:" model.levelTwo
        , Element.map LevelThree <| Counter.view "third Level:" model.levelThree
        , Element.map Missed <| Counter.view "missed:" model.missed
        , createButton SpunRoulette "spun cycles 3-5?" <| buttonState model.spunRoulette
        , createButton ColorRoulette "spun to correct color?" <| buttonState model.colorRoulette
        ]


decoration : List (Element.Attribute Msg)
decoration =
    [ padding 20
    , centerX
    , centerY
    ]
