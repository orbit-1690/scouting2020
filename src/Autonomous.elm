module Autonomous exposing (Model, Msg, getter, init, update, view)

import Array
import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, image, padding, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (bold, center)
import Element.Input as Input exposing (button, radioRow)
import GetMatch
import Html.Attributes exposing (style)
import TeamData exposing (stationToString)


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
    , teamData : TeamData.Model
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


forOptionWith : String -> Input.OptionState -> Element.Element Msg
forOptionWith displayedText option =
    el
        (case option of
            Input.Idle ->
                [ Font.color Colors.gray ]

            Input.Focused ->
                [ Font.color black ]

            Input.Selected ->
                [ Font.bold ]
        )
    <|
        text displayedText


view : Model -> Element.Element Msg
view model =
    let
        createButton : Element.Element Msg
        createButton =
            button
                [ Font.size 70
                , centerX
                ]
                { onPress = Just Moved
                , label =
                    Element.row [ spacing 30 ]
                        [ text "moved?"
                        , image [ width <| Element.maximum 70 fill ]
                            { description = ""
                            , src =
                                if model.moved then
                                    "https://i.imgur.com/eiuQZig.png"

                                else
                                    "https://i.imgur.com/SeSMGGI.png"
                            }
                        ]
                }

        radios :
            Input.Label Msg
            -> BallsInitAmount
            -> (Input.OptionState -> Element.Element Msg)
            -> BallsInitAmount
            -> (Input.OptionState -> Element.Element Msg)
            -> Element.Element Msg
        radios label ballAmount1 txtMsg1 ballAmount2 txtMsg2 =
            radioRow
                [ padding 10
                , spacing 50
                , Font.size 67
                ]
                { onChange = BallsAmount
                , selected = Just model.ballsAmount
                , label = label
                , options =
                    [ Input.optionWith ballAmount1 <| txtMsg1
                    , Input.optionWith ballAmount2 <| txtMsg2
                    ]
                }
    in
    column
        [ width fill
        , spacing 80
        , Element.height <| Element.fillPortion 5
        ]
        [ column [ centerX, spacing 30 ]
            [ radios
                (Input.labelAbove
                    [ Font.size 70
                    , padding 20
                    , spacing 20
                    , Font.underline
                    , centerX
                    ]
                 <|
                    text "started with:"
                )
                NoBalls
                (forOptionWith "0 balls")
                OneBall
                (forOptionWith "1 ball")
            , radios
                (Input.labelHidden "option2")
                TwoBalls
                (forOptionWith "2 balls")
                ThreeBalls
                (forOptionWith "3 balls")
            ]
        , createButton
        , column
            [ Font.size 60
            , spacing 60
            , padding 20
            , fontExternal
            , heightPercent 50
            ]
            [ column
                [ spacing 50
                , heightPercent 70
                ]
                [ Element.map LevelThree <| Counter.view "third Level:" model.levelThree
                , Element.map LevelTwo <| Counter.view "second Level:" model.levelTwo
                , Element.map Missed <| Counter.view "missed:" model.missed
                , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
                ]
            , text "Collected from:"
                |> el
                    [ Font.size 69
                    , Font.underline
                    ]
            , Element.map TrenchCollection <| Counter.view "their trench:" model.trenchCollection
            , Element.map EnemyTrenchCollection <| Counter.view "enemy's trench:" model.enemyTrenchCollection
            , Element.map RendezvousCollection <| Counter.view "rendezvous:" model.rendezvousCollection
            ]
        ]


init : Array.Array GetMatch.Match -> Model
init match =
    Model NoBalls False Counter.init Counter.init Counter.init Counter.init Counter.init Counter.init Counter.init (TeamData.init match)


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
            { model | trenchCollection = max (maxThrownOut + model.trenchCollection - maxCollected) <| counterUpdate count model.trenchCollection }

        BallsAmount ballsAmount ->
            if maxCollected - maxThrownOut < ballsAmountToInt model.ballsAmount - ballsAmountToInt ballsAmount then
                model

            else
                { model | ballsAmount = ballsAmount }

        EnemyTrenchCollection count ->
            { model | enemyTrenchCollection = max (maxThrownOut + model.enemyTrenchCollection - maxCollected) <| counterUpdate count model.enemyTrenchCollection }

        RendezvousCollection count ->
            { model | rendezvousCollection = max (maxThrownOut + model.rendezvousCollection - maxCollected) <| counterUpdate count model.rendezvousCollection }


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
