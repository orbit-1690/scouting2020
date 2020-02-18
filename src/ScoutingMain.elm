module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous
import Browser
import Climbing
import Colors exposing (blue, purple, white)
import Element exposing (centerX, centerY, column, el, fill, fillPortion, height, htmlAttribute, image, layout, padding, row, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import File.Download as Download
import GetMatch
import Html.Attributes exposing (style)
import Result.Extra exposing (merge)
import TeamData
import Teleop


main : Program () Model Msg
main =
    Browser.element
        { init = always ( init, Cmd.none )
        , view = view >> layout [ width fill, height fill, htmlAttribute <| style "touch-action" "manipulation" ]
        , update = update
        , subscriptions = always Sub.none
        }


widthPercent : Int -> Element.Attribute Msg
widthPercent percent =
    htmlAttribute << style "width" <| String.fromInt percent ++ "%"


heightPercent : Int -> Element.Attribute Msg
heightPercent percent =
    htmlAttribute << style "height" <| String.fromInt percent ++ "%"


type Pages
    = TeamDataPage
    | AutonomousPage
    | TeleopPage
    | ClimbingPage
    | SubmitPage


type Msg
    = TeamDataMsg TeamData.Msg
    | AutonomousDataMsg Autonomous.Msg
    | TeleopDataMsg Teleop.Msg
    | ClimbingDataMsg Climbing.Msg
    | SubmitDataMsg Msg
    | PrevPage
    | NextPage
    | Submit
    | YesSubmit
    | NoSubmit


type PagePosition
    = FirstPage
    | MiddlePage
    | LastPage
    | SubmitPosPage


type alias Model =
    { teamData : TeamData.Model
    , autonomousData : Autonomous.Model
    , teleopData : Teleop.Model
    , climbingData : Climbing.Model
    , pages : Pages
    }


findColor : String -> Element.Color
findColor alliance =
    if String.contains "Blue" alliance then
        Colors.backgroundBlue

    else if String.contains "Red" alliance then
        Colors.backgroundRed

    else
        purple


stylishPage : PagePosition -> Element.Element Msg
stylishPage position =
    let
        createButtons : Msg -> Element.Element Msg -> Msg -> Element.Element Msg -> Element.Element Msg
        createButtons firstMsg firstLabelMsg secondMsg secondLabelMsg =
            row
                [ spacing 15
                , width fill
                , centerX
                , centerY
                , Font.size 40
                ]
                [ button
                    buttonStyle
                    { onPress = Just <| firstMsg
                    , label = firstLabelMsg
                    }
                , el [ width fill ] <| text ""
                , button
                    buttonStyle
                    { onPress = Just <| secondMsg
                    , label = secondLabelMsg
                    }
                ]

        imageLabel : String -> String -> Element.Element Msg
        imageLabel description src =
            image [ Font.size 20, width <| Element.maximum 100 fill ]
                { description = description, src = src }

        nextPageImage : String
        nextPageImage =
            "https://i.imgur.com/NFxo6mv.png"

        preiousPageImage : String
        preiousPageImage =
            "https://i.imgur.com/mM68bja.png"
    in
    (case position of
        FirstPage ->
            row
                [ width fill, height fill ]
                [ el [ width fill ] <| text ""
                , el [ width fill ] <| text ""
                , button
                    buttonStyle
                    { onPress = Just <| NextPage
                    , label =
                        imageLabel "Next Page" nextPageImage
                    }
                ]

        LastPage ->
            createButtons
                PrevPage
                (imageLabel "Previous Page" preiousPageImage)
                Submit
            <|
                el [ Font.size 70, centerX ] (text "Submit")

        MiddlePage ->
            createButtons PrevPage
                (imageLabel "Previous Page" preiousPageImage)
                NextPage
                (imageLabel "Next Page" nextPageImage)

        SubmitPosPage ->
            createButtons YesSubmit
                (el [ Font.size 70, centerX ] (text "Yes"))
                NoSubmit
            <|
                el [ Font.size 70, centerX ] (text "No")
    )
        |> el
            [ centerX
            , width fill
            ]


init : Model
init =
    { teamData = TeamData.init <| Array.fromList GetMatch.matches
    , autonomousData = Autonomous.init
    , teleopData = Teleop.init
    , climbingData = Climbing.init
    , pages = TeamDataPage
    }


dumpModel : Model -> Cmd Msg
dumpModel model =
    Download.string
        (String.concat [ String.join "-" <| TeamData.getter model.teamData, ".csv" ])
        "content/text"
    <|
        String.join "\n"
            [ String.join "\n" <| TeamData.getter model.teamData
            , Autonomous.getter model.autonomousData
            , Teleop.getter model.teleopData
            , Climbing.getter model.climbingData
            ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TeamDataMsg teamMsg ->
            ( { model | teamData = TeamData.update teamMsg model.teamData }, Cmd.none )

        AutonomousDataMsg autoMsg ->
            ( { model | autonomousData = Autonomous.update autoMsg model.autonomousData }, Cmd.none )

        TeleopDataMsg telMsg ->
            ( { model | teleopData = Teleop.update telMsg model.teleopData }, Cmd.none )

        ClimbingDataMsg climbMsg ->
            ( { model | climbingData = Climbing.update climbMsg model.climbingData }, Cmd.none )

        PrevPage ->
            ( case model.pages of
                AutonomousPage ->
                    { model | pages = TeamDataPage }

                TeleopPage ->
                    { model | pages = AutonomousPage }

                ClimbingPage ->
                    { model | pages = TeleopPage }

                SubmitPage ->
                    model

                TeamDataPage ->
                    model
            , Cmd.none
            )

        NextPage ->
            let
                matchError : Result String GetMatch.Match
                matchError =
                    TeamData.getMatch model.teamData

                stationError : Result String Int
                stationError =
                    TeamData.getTeam model.teamData

                verifier : Bool
                verifier =
                    (not <| List.member matchError [ Err "No such match", Err "Invalid match number" ])
                        && stationError
                        /= Err "No station"
                        && (not << String.isEmpty << .scouterName << .teamData) model
                        || List.member model.teamData.scouterName [ "Itamar", "tom", "hadar", "shira" ]
            in
            ( if model.pages == TeamDataPage && verifier then
                { model | pages = AutonomousPage }

              else if model.pages == AutonomousPage then
                { model | pages = TeleopPage }

              else if model.pages == TeleopPage then
                { model | pages = ClimbingPage }

              else
                model
            , Cmd.none
            )

        YesSubmit ->
            ( model, dumpModel model )

        NoSubmit ->
            ( { model | pages = ClimbingPage }, Cmd.none )

        Submit ->
            ( { model | pages = SubmitPage }, Cmd.none )

        SubmitDataMsg _ ->
            ( model, Cmd.none )


teamDataToString : Model -> String
teamDataToString model =
    model.teamData
        |> TeamData.getTeam
        |> Result.map String.fromInt
        |> merge


view : Model -> Element.Element Msg
view model =
    let
        page : String -> PagePosition -> Element.Element Msg -> Element.Element Msg
        page name pagePosition msg =
            column
                [ centerX
                , width fill
                , Background.color << findColor <| TeamData.stationToString model.teamData.station
                , height fill
                ]
                [ column
                    [ heightPercent 9
                    , width fill
                    ]
                    [ text name
                        |> el
                            [ Font.size 55
                            , Font.color Colors.white
                            , Font.glow Colors.black 10
                            , centerX
                            , height <| fillPortion 1
                            , Font.underline
                            ]
                    , text ("scouted team: " ++ teamDataToString model)
                        |> el
                            [ Font.size 47
                            , Font.color Colors.white
                            , Font.glow Colors.black 10
                            , centerX
                            , Font.bold
                            , height <| fillPortion 1
                            ]
                    ]
                , msg
                , stylishPage pagePosition
                ]
    in
    case model.pages of
        TeamDataPage ->
            model.teamData
                |> TeamData.view
                |> Element.map TeamDataMsg
                |> page "Registration" FirstPage

        AutonomousPage ->
            Autonomous.view model.autonomousData
                |> Element.map AutonomousDataMsg
                |> page "Autonomous" MiddlePage

        TeleopPage ->
            Teleop.view model.teleopData
                |> Element.map TeleopDataMsg
                |> page "Teleop" MiddlePage

        ClimbingPage ->
            Climbing.view model.climbingData
                |> Element.map ClimbingDataMsg
                |> page "End-game" LastPage

        SubmitPage ->
            page
                "Submit"
                SubmitPosPage
                << Element.map SubmitDataMsg
            <|
                column [ centerY, centerX ]
                    [ el [ Font.size 70, centerX, centerY ]
                        (text "Are you sure")
                    , el [ Font.size 70, centerX, centerY ]
                        (text "you want to submit?")
                    ]


buttonStyle : List (Element.Attribute Msg)
buttonStyle =
    [ Font.size 40
    , Font.glow blue 5
    , Border.rounded 10
    , centerX
    , centerY
    , height fill
    , width fill
    ]


fontExternal : Element.Attr () Msg
fontExternal =
    Font.family
        [ Font.external
            { name = "Open Sans"
            , url = "https://fonts.googleapis.com/css?family=Open+Sans:400i&display=swap"
            }
        ]
