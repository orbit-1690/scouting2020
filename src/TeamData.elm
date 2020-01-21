module TeamData exposing (Model, Msg, init, subscriptions, teamDataView, update)

import Colors exposing (black, blue, orange, pink, red, sky, white, yellow)
import Element exposing (centerX, centerY, column, fill, height, minimum, padding, px, spacing, width)
import Element.Background as Background
import Element.Border as Border exposing (rounded, widthXY)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (labelHidden)
import GetMatch exposing (getMatch, maybeIntToInt, unwrapToString)
import String


type Msg
    = ScouterInput String
    | StationInput String
    | MatchInput String


type alias Model =
    { scouterName : String
    , station : String
    , match : Maybe Int
    }


teamDataView : Model -> Element.Element Msg
teamDataView model =
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
        [ textInput model.scouterName ScouterInput "Scouter's name"
        , textInput model.station StationInput "Scouted station"
        , textInput (unwrapToString model.match) MatchInput "Match number"
        , Element.el
            [ Background.color orange
            , width <| minimum 350 <| fill
            , height fill
            , center
            , Font.color white
            , Font.glow blue 5
            , Font.size 20
            , rounded 3
            , Font.family
                [ Font.external
                    { name = "Open Sans"
                    , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                    }
                ]
            ]
            (Element.text <| getMatch model.match model.station)
        ]


textInput : String -> (String -> Msg) -> String -> Element.Element Msg
textInput modelValue nextButton name =
    Input.text
        [ Font.color sky
        , Font.size 20
        , height fill
        , Font.family
            [ Font.external
                { name = "Open Sans"
                , url = "https://fonts.googleapis.com/css?family=Open+Sans:700i&display=swap"
                }
            ]
        ]
        { onChange = nextButton
        , text = modelValue
        , placeholder = Just <| Input.placeholder [] <| Element.text name
        , label = labelHidden modelValue
        }


init : Model
init =
    Model "" "" Nothing


update : Msg -> Model -> Model
update msg model =
    case msg of
        ScouterInput s ->
            { model | scouterName = s }

        StationInput s ->
            { model | station = s }

        MatchInput s ->
            { model | match = String.toInt s }


subscriptions : Sub Msg
subscriptions =
    Sub.none
