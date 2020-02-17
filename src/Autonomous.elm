module Autonomous exposing (Model, Msg, getter, init, update, view)

import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, radioRow)
import Html.Attributes exposing (style)


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
        , Font.size 60
        , Font.glow blue 5
        , Border.rounded 4
        , Font.bold
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
                }
            ]
        , Background.color purple
        , centerX
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
    let
        radios :
            Input.Label Msg
            -> BallsInitAmount
            -> Element.Element Msg
            -> BallsInitAmount
            -> Element.Element Msg
            -> Element.Element Msg
        radios label ballAmount1 txtMsg1 ballAmount2 txtMsg2 =
            radioRow
                [ padding 10
                , spacing 50
                , Font.size 55
                , heightPercent 10
                , Font.semiBold
                ]
                { onChange = BallsAmount
                , selected = Just model.ballsAmount
                , label = label
                , options =
                    [ Input.option ballAmount1 <| txtMsg1
                    , Input.option ballAmount2 <| txtMsg2
                    ]
                }
    in
    column
        [ Background.color blue
        , centerX
        , centerY
        , width fill
        , heightPercent 40
        , Element.height <| Element.fillPortion 5
        ]
        [ radios
            (Input.labelAbove
                [ Font.size 60
                , padding 20
                , spacing 20
                , Font.underline
                ]
             <|
                text "started with:"
            )
            NoBalls
            (text "0 balls")
            OneBall
            (text "1 ball")
        , radios
            (Input.labelHidden "option2")
            TwoBalls
            (text "2 balls")
            ThreeBalls
            (text "3 balls")
        , createButton Moved <|
            if model.moved then
                "moved."

            else
                "moved?"
        , column
            [ Font.size 50
            , spacing 20
            , padding 20
            , Font.semiBold
            , fontExternal
            , heightPercent 50
            ]
            [ column
                [ spacing 20
                , heightPercent 70
                ]
                [ Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
                , Element.map LevelTwo <| Counter.view "second Level:" model.levelTwo
                , Element.map LevelThree <| Counter.view "third Level:" model.levelThree
                , Element.map Missed <| Counter.view "missed:" model.missed
                ]
            , text "Collected from:"
                |> el
                    [ Font.size 65
                    , Font.underline
                    ]
            , Element.map TrenchCollection <| Counter.view "their trench:" model.trenchCollection
            , Element.map EnemyTrenchCollection <| Counter.view "enemy's trench:" model.enemyTrenchCollection
            , Element.map RendezvousCollection <| Counter.view "rendezvous:" model.rendezvousCollection
            ]
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


fontExternal : Element.Attr () Msg
fontExternal =
    Font.family
        [ Font.external
            { name = "Open Sans"
            , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
            }
        ]


heightPercent : Int -> Element.Attribute Msg
heightPercent percent =
    htmlAttribute << style "height" <| String.fromInt percent ++ "%"
