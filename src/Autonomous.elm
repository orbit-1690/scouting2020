module Autonomous exposing (Balls(..), Model, Msg, autonomousView, init, subscriptions, update)

import Colors exposing (black, blue, blueGreen, lightBlue, orange, pink, purple, sky, white, yellow)
import Counter
import Element exposing (centerX, centerY, column, el, fill, height, minimum, padding, px, rgb, spacing, text, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (button, labelHidden, radioRow)


type Msg
    = Moved
    | LowLevel Counter.Msg
    | HighLevel Counter.Msg
    | Missed Counter.Msg
    | TrenchCollection Counter.Msg
    | EnemyTrenchCollection Counter.Msg
    | RendezvousCollection Counter.Msg
    | Stage Balls


type alias Model =
    { stage : Balls
    , moved : Bool
    , lowlevel : Counter.Model
    , highlevel : Counter.Model
    , missed : Counter.Model
    , trenchCollection : Counter.Model
    , enemyTrenchCollection : Counter.Model
    , rendezvousCollection : Counter.Model
    }


type Balls
    = Ball0
    | Ball1
    | Ball2
    | Ball3


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
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
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


autonomousView : Model -> Element.Element Msg
autonomousView model =
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
            , spacing 20
            ]
            { onChange = Stage
            , selected = Just model.stage
            , label = Input.labelAbove [] (text "started with:")
            , options =
                [ Input.option Ball0 (text "0 balls")
                , Input.option Ball1 (text "1 ball")
                , Input.option Ball2 (text "2 balls")
                , Input.option Ball3 (text "3 balls")
                ]
            }
        , createButton Moved "moved?"
        , printButton "didn't move" "moved" model.moved
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map HighLevel <| Counter.view "high Level:" model.highlevel
        , Element.map Missed <| Counter.view "missed:" model.missed
        , Element.map TrenchCollection <| Counter.view "Collected from their trench:" model.trenchCollection
        , Element.map EnemyTrenchCollection <| Counter.view "Collected from enemy's trench:" model.enemyTrenchCollection
        , Element.map RendezvousCollection <| Counter.view "Collected from rendemockelk881\u{0003}#:" model.rendezvousCollection
        ]


init : Model
init =
    Model Ball0 False Counter.init Counter.init Counter.init Counter.init Counter.init <| Counter.init


update : Msg -> Model -> Model
update msg model =
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LowLevel count ->
            { model | lowlevel = Counter.update 15 0 count model.lowlevel }

        HighLevel count ->
            { model | highlevel = Counter.update 15 0 count model.highlevel }

        Missed count ->
            { model | missed = Counter.update 15 0 count model.missed }

        TrenchCollection count ->
            { model | trenchCollection = Counter.update 15 0 count model.trenchCollection }

        Stage stage ->
            { model | stage = stage }

        EnemyTrenchCollection count ->
            { model | enemyTrenchCollection = Counter.update 15 0 count model.enemyTrenchCollection }

        RendezvousCollection count ->
            { model | rendezvousCollection = Counter.update 15 0 count model.rendezvousCollection }


subscriptions : Sub Msg
subscriptions =
    Sub.none
