module Autonomous exposing (Model, Msg, getter, init, update, view)

import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, padding, spacing, text)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, radioRow)


type Msg
    = Moved
    | LowLevel Counter.Msg
    | LevelTwo Counter.Msg
    | LevelThree Counter.Msg
    | Missed Counter.Msg
    | TrenchCollection Counter.Msg
    | EnemyTrenchCollection Counter.Msg
    | RendezvousCollection Counter.Msg
    | BallsAmount BallsInitAmount


type alias Model =
    { ballsAmount : BallsInitAmount
    , moved : Bool
    , lowlevel : Counter.Model
    , levelTwo : Counter.Model
    , levelThree : Counter.Model
    , missed : Counter.Model
    , trenchCollection : Counter.Model
    , enemyTrenchCollection : Counter.Model
    , rendezvousCollection : Counter.Model
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

        ballsAmountToString : BallsInitAmount -> String
        ballsAmountToString ball =
            case ball of
                NoBalls ->
                    "none"

                OneBall ->
                    "1"

                TwoBalls ->
                    "2"

                ThreeBalls ->
                    "3"
    in
    String.join "\n"
        [ "started with" ++ "," ++ ballsAmountToString model.ballsAmount
        , "moved?" ++ "," ++ boolToString model.moved
        , "level 1" ++ "," ++ String.fromInt model.lowlevel
        , "level 2" ++ "," ++ String.fromInt model.levelTwo
        , "level 3" ++ "," ++ String.fromInt model.levelThree
        , "missed" ++ "," ++ String.fromInt model.missed
        , "trenchCollection" ++ "," ++ String.fromInt model.trenchCollection
        , "enemyTrenchCollection" ++ "," ++ String.fromInt model.enemyTrenchCollection
        , "rendezvousCollection" ++ "," ++ String.fromInt model.rendezvousCollection
        ]


type BallsInitAmount
    = NoBalls
    | OneBall
    | TwoBalls
    | ThreeBalls


createButton : Msg -> String -> Element.Element Msg
createButton msg name =
    button
        [ Font.color white
        , Font.size 25
        , Font.glow blue 5
        , Border.rounded 4
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


buttonInfo : String -> String -> Bool -> Element.Element Msg
buttonInfo onFalse onTrue modelBool =
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
        [ radioRow
            [ padding 10
            , spacing 50
            , Font.size 60
            ]
            { onChange = BallsAmount
            , selected = Just model.ballsAmount
            , label = Input.labelAbove [ Font.size 60, padding 10, spacing 20 ] <| text "started with:"
            , options =
                [ Input.option NoBalls <| text "0 balls"
                , Input.option OneBall <| text "1 ball"
                ]
            }
        , radioRow
            [ padding 10
            , spacing 50
            , Font.size 60
            ]
            { onChange = BallsAmount
            , selected = Just model.ballsAmount
            , label = Input.labelHidden "option2"
            , options =
                [ Input.option TwoBalls <| text "2 balls"
                , Input.option ThreeBalls <| text "3 balls"
                ]
            }
        , createButton Moved "moved?"
        , buttonInfo "didn't move" "moved" model.moved
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map LevelTwo <| Counter.view "second Level:" model.levelTwo
        , Element.map LevelThree <| Counter.view "third Level:" model.levelThree
        , Element.map Missed <| Counter.view "missed:" model.missed
        , Element.map TrenchCollection <| Counter.view "Collected from their trench:" model.trenchCollection
        , Element.map EnemyTrenchCollection <| Counter.view "Collected from enemy's trench:" model.enemyTrenchCollection
        , Element.map RendezvousCollection <| Counter.view "Collected from rendezvous:" model.rendezvousCollection
        ]


init : Model
init =
    Model NoBalls False Counter.init Counter.init Counter.init Counter.init Counter.init Counter.init <| Counter.init


update : Msg -> Model -> Model
update msg model =
    let
        counterUpdate : Counter.Msg -> Counter.Model -> Counter.Model
        counterUpdate =
            Counter.update

        ballsAmountToInt : BallsInitAmount -> Int
        ballsAmountToInt ball =
            case ball of
                NoBalls ->
                    0

                OneBall ->
                    1

                TwoBalls ->
                    2

                ThreeBalls ->
                    3

        maxCollected : Int
        maxCollected =
            model.trenchCollection + model.enemyTrenchCollection + model.rendezvousCollection + ballsAmountToInt model.ballsAmount

        maxThrownOut : Int
        maxThrownOut =
            model.lowlevel + model.levelTwo + model.levelThree + model.missed
    in
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LowLevel count ->
            { model | lowlevel = min (maxCollected - maxThrownOut + model.lowlevel) <| counterUpdate count model.lowlevel }

        LevelTwo count ->
            { model | levelTwo = min (maxCollected - maxThrownOut + model.levelTwo) <| counterUpdate count model.levelTwo }

        LevelThree count ->
            { model | levelThree = min (maxCollected - maxThrownOut + model.levelThree) <| counterUpdate count model.levelThree }

        Missed count ->
            { model | missed = min (maxCollected - maxThrownOut + model.missed) <| counterUpdate count model.missed }

        TrenchCollection count ->
            { model | trenchCollection = counterUpdate count model.trenchCollection }

        BallsAmount ballsAmount ->
            if maxCollected - maxThrownOut < ballsAmountToInt model.ballsAmount - ballsAmountToInt ballsAmount then
                model

            else
                { model | ballsAmount = ballsAmount }

        EnemyTrenchCollection count ->
            { model | enemyTrenchCollection = counterUpdate count model.enemyTrenchCollection }

        RendezvousCollection count ->
            { model | rendezvousCollection = counterUpdate count model.rendezvousCollection }
