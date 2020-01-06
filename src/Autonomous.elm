module Autonomous exposing (Model, Msg, init, subscriptions, update, view)

import Bool.Extra exposing (toString)
import Counter
import Element exposing (column, text)
import Element.Font as Font
import Element.Input as Input exposing (button, labelHidden)
import Http
import Maybe
import String


type Msg
    = Moved
    | LevelOne Counter.Msg
    | LevelTow Counter.Msg
    | LevelThree Counter.Msg


type alias Model =
    { moved : Bool
    , levelOne : Counter.Model
    , levelTow : Counter.Model
    , levelThree : Counter.Model
    }


autonomousView : Model -> Element.Element Msg
autonomousView model =
    column []
        [ button [] { onPress = Just Moved, label = text "moved?" }
        , text <| toString model.moved
        , Element.map LevelOne <| Counter.view model.levelOne
        , Element.map LevelTow <| Counter.view model.levelTow
        , Element.map LevelThree <| Counter.view model.levelThree
        ]


init : Bool -> Counter.Model -> Counter.Model -> Counter.Model -> Model
init =
    Model


update : Msg -> Model -> Model
update msg model =
    case msg of
        Moved ->
            { model | moved = not model.moved }

        LevelOne count1 ->
            { model | levelOne = Counter.update 3 0 count1 model.levelOne }

        LevelTow count2 ->
            { model | levelTow = Counter.update 3 0 count2 model.levelTow }

        LevelThree count3 ->
            { model | levelThree = Counter.update 3 0 count3 model.levelThree }


view : Model -> Element.Element Msg
view model =
    autonomousView model


subscriptions : Sub Msg
subscriptions =
    Sub.none
