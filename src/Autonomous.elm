module Autonomous exposing (BallsInitAmount(..), Model, Msg, init, update, view)

import Array
import Colors exposing (black, blue, purple, sky, white)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, htmlAttribute, image, maximum, padding, spacing, text, width)
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
    | RedTrenchCollection Counter.Msg
    | BlueTrenchCollection Counter.Msg
    | RendezvousCollection Counter.Msg
    | BallsAmount BallsInitAmount


type alias Model =
    { ballsAmount : BallsInitAmount
    , moved : Bool
    , lowlevel : Counter.Model
    , levelTwo : Counter.Model
    , levelThree : Counter.Model
    , missed : Counter.Model
    , redTrenchCollection : Counter.Model
    , blueTrenchCollection : Counter.Model
    , rendezvousCollection : Counter.Model
    , teamData : TeamData.Model
    }


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
                [ Font.color Colors.gray, center ]

            Input.Focused ->
                [ Font.color black ]

            Input.Selected ->
                [ Font.bold, center ]
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
    in
    column
        [ width fill
        , spacing 60
        , Element.height <| Element.fillPortion 5
        ]
        [ el [ centerX ] <|
            radioRow
                [ padding 10
                , spacing 80
                , Font.size 60
                ]
                { onChange = BallsAmount
                , selected = Just model.ballsAmount
                , label =
                    Input.labelHidden ""
                , options =
                    [ Input.optionWith NoBalls (forOptionWith "0\nballs")
                    , Input.optionWith OneBall (forOptionWith "1\nball")
                    , Input.optionWith TwoBalls (forOptionWith "2\nballs")
                    , Input.optionWith ThreeBalls (forOptionWith "3\nballs")
                    ]
                }
        , createButton
        , column
            [ Font.size 60
            , spacing 50
            , Element.paddingXY 100 0
            , fontExternal
            , width fill
            , heightPercent 50
            ]
            [ column
                [ spacing 30
                , heightPercent 65
                , width fill
                ]
                [ Element.map LevelThree <| Counter.view (text "third Level:") model.levelThree
                , Element.map LevelTwo <| Counter.view (text "second Level:") model.levelTwo
                , Element.map Missed <| Counter.view (text "missed:") model.missed
                , Element.map LowLevel <| Counter.view (text "low Level:") model.lowlevel
                ]
            , text "Collected from:"
                |> el
                    [ Font.size 70
                    , Font.underline
                    , centerX
                    ]
            , column
                [ spacing 30
                , heightPercent 65
                , width fill
                ]
                [ Element.map RedTrenchCollection <|
                    Counter.view
                        (image
                            [ height <|
                                maximum 200 fill
                            ]
                            { src = "https://i.imgur.com/04kGJ8A.jpg", description = "" }
                        )
                        model.redTrenchCollection
                , Element.map BlueTrenchCollection <|
                    Counter.view
                        (image
                            [ height <|
                                maximum 200 fill
                            , Background.color blue
                            ]
                            { src = "https://i.imgur.com/5BxqoZK.jpg", description = "" }
                        )
                        model.blueTrenchCollection
                , Element.map RendezvousCollection <|
                    Counter.view
                        (image
                            [ height <|
                                maximum 200 fill
                            ]
                            { src = "https://i.imgur.com/yvjl4Dt.png", description = "" }
                        )
                        model.rendezvousCollection
                ]
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
            model.redTrenchCollection + model.blueTrenchCollection + model.rendezvousCollection + ballsAmountToInt model.ballsAmount

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

        RedTrenchCollection count ->
            { model | redTrenchCollection = max (maxThrownOut + model.redTrenchCollection - maxCollected) <| counterUpdate count model.redTrenchCollection }

        BallsAmount ballsAmount ->
            if maxCollected - maxThrownOut < ballsAmountToInt model.ballsAmount - ballsAmountToInt ballsAmount then
                model

            else
                { model | ballsAmount = ballsAmount }

        BlueTrenchCollection count ->
            { model | blueTrenchCollection = max (maxThrownOut + model.blueTrenchCollection - maxCollected) <| counterUpdate count model.blueTrenchCollection }

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
