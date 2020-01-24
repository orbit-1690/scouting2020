module Autonomous exposing (Model, Msg, init, subscriptions, update, view)

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
    | HighLevel Counter.Msg
    | Missed Counter.Msg
    | TrenchCollection Counter.Msg
    | EnemyTrenchCollection Counter.Msg
    | RendezvousCollection Counter.Msg
    | BallsAmount BallsInitAmount


type alias Model =
    { ballsAmount : BallsInitAmount
    , moved : Bool
    , lowlevel : Counter.Model
    , highlevel : Counter.Model
    , missed : Counter.Model
    , trenchCollection : Counter.Model
    , enemyTrenchCollection : Counter.Model
    , rendezvousCollection : Counter.Model
    }


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
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
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
            , spacing 20
            ]
            { onChange = BallsAmount
            , selected = Just model.ballsAmount
            , label = Input.labelAbove [] (text "started with:")
            , options =
                [ Input.option NoBalls (text "0 balls")
                , Input.option OneBall (text "1 ball")
                , Input.option TwoBalls (text "2 balls")
                , Input.option ThreeBalls (text "3 balls")
                ]
            }
        , createButton Moved "moved?"
        , buttonInfo "didn't move" "moved" model.moved
        , Element.map LowLevel <| Counter.view "low Level:" model.lowlevel
        , Element.map HighLevel <| Counter.view "high Level:" model.highlevel
        , Element.map Missed <| Counter.view "missed:" model.missed
        , Element.map TrenchCollection <| Counter.view "Collected from their trench:" model.trenchCollection
        , Element.map EnemyTrenchCollection <| Counter.view "Collected from enemy's trench:" model.enemyTrenchCollection
        , Element.map RendezvousCollection <| Counter.view "Collected from rendezvous:" model.rendezvousCollection
        ]


init : Model
init =
    Model NoBalls False Counter.init Counter.init Counter.init Counter.init Counter.init <| Counter.init


update : Msg -> Model -> Model
update msg model =
    let
        counterUpdate : Counter.Msg -> Counter.Model -> Counter.Model
        counterUpdate =
            Counter.update 15
    in
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LowLevel count ->
            { model | lowlevel = counterUpdate count model.lowlevel }

        HighLevel count ->
            { model | highlevel = counterUpdate count model.highlevel }

        Missed count ->
            { model | missed = counterUpdate count model.missed }

        TrenchCollection count ->
            { model | trenchCollection = counterUpdate count model.trenchCollection }

        BallsAmount ballsAmount ->
            { model | ballsAmount = ballsAmount }

        EnemyTrenchCollection count ->
            { model | enemyTrenchCollection = counterUpdate count model.enemyTrenchCollection }

        RendezvousCollection count ->
            { model | rendezvousCollection = counterUpdate count model.rendezvousCollection }


subscriptions : Sub Msg
subscriptions =
    Sub.none
