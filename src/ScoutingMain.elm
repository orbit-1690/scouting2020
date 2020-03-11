module ScoutingMain exposing (Model, Msg, init, update, view)

import Array
import Autonomous exposing (BallsInitAmount(..))
import Browser
import Climbing exposing (Status(..))
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
import TeamData exposing (currentMatch, getTeam, stationToString)
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
    | ClearPages
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
    , askForClear : Bool
    }


findColor : String -> ( Element.Color, Element.Color )
findColor alliance =
    if String.contains "Blue" alliance then
        ( Colors.backgroundBlue, Colors.blue )

    else if String.contains "Red" alliance then
        ( Colors.backgroundRed, Colors.red )

    else
        ( purple, Colors.white )


stylishPage : PagePosition -> Bool -> Element.Element Msg
stylishPage position isClearButtonPressed =
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
            "https://i.imgur.com/CFiCTTX.png"

        preiousPageImage : String
        preiousPageImage =
            "https://i.imgur.com/xegW3W8.png"
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
            column [ width fill ]
                [ if isClearButtonPressed then
                    button [ Font.size 90, Font.glow blue 5, centerX ] { onPress = Just ClearPages, label = text "Clear" }

                  else
                    Element.none
                , createButtons YesSubmit
                    (el [ Font.size 70, centerX ] (text "Yes"))
                    NoSubmit
                  <|
                    el [ Font.size 70, centerX ] (text "No")
                ]
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
    , askForClear = False
    }


dumpModel : Model -> Cmd Msg
dumpModel model =
    let
        teamDataGetter : List String
        teamDataGetter =
            [ "match" ++ "," ++ model.teamData.matchNumber
            , "isRematch"
                ++ ","
                ++ (if model.teamData.isRematch then
                        "1"

                    else
                        "0"
                   )
            , "station" ++ "," ++ stationToString model.teamData.station
            , "team"
                ++ ","
                ++ (getTeam model.teamData
                        |> Result.map String.fromInt
                        |> merge
                   )
            , "scouterName" ++ "," ++ "'" ++ model.teamData.scouterName ++ "'"
            ]

        autonomousGetter : String
        autonomousGetter =
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
                [ "started with" ++ "," ++ ballsAmountToString model.autonomousData.ballsAmount
                , "moved?" ++ "," ++ boolToString model.autonomousData.moved
                , "level 1" ++ "," ++ String.fromInt model.autonomousData.lowlevel
                , "level 2" ++ "," ++ String.fromInt model.autonomousData.levelTwo
                , "level 3" ++ "," ++ String.fromInt model.autonomousData.levelThree
                , "missed" ++ "," ++ String.fromInt model.autonomousData.missed
                , if String.contains "Blue" <| TeamData.stationToString model.teamData.station then
                    String.join "\n"
                        [ "trenchCollection" ++ "," ++ String.fromInt model.autonomousData.blueTrenchCollection
                        , "enemyTrenchCollection" ++ "," ++ String.fromInt model.autonomousData.redTrenchCollection
                        ]

                  else
                    String.join "\n"
                        [ "trenchCollection" ++ "," ++ String.fromInt model.autonomousData.redTrenchCollection
                        , "enemyTrenchCollection" ++ "," ++ String.fromInt model.autonomousData.blueTrenchCollection
                        ]
                , "rendezvousCollection" ++ "," ++ String.fromInt model.autonomousData.rendezvousCollection
                ]

        teleopGetter : String
        teleopGetter =
            let
                boolToString : Bool -> String
                boolToString bool =
                    if bool then
                        "1"

                    else
                        "0"
            in
            String.join "\n"
                [ "colorRoulette" ++ "," ++ boolToString model.teleopData.colorRoulette
                , "spunRoulette?" ++ "," ++ boolToString model.teleopData.spunRoulette
                , "level 1." ++ "," ++ String.fromInt model.teleopData.lowlevel
                , "level 2." ++ "," ++ String.fromInt model.teleopData.levelTwo
                , "level 3." ++ "," ++ String.fromInt model.teleopData.levelThree
                , "missed." ++ "," ++ String.fromInt model.teleopData.missed
                ]

        climbingGetter : String
        climbingGetter =
            let
                boolToString : Bool -> String
                boolToString bool =
                    if bool then
                        "1"

                    else
                        "0"

                statusToString : Status -> String
                statusToString status =
                    case status of
                        Hanged ->
                            "Hanged"

                        Parked ->
                            "Parked"

                        Loser ->
                            "Loser"
            in
            String.join "\n"
                [ "triedClimb" ++ "," ++ boolToString model.climbingData.triedClimb
                , "balanced?" ++ "," ++ boolToString model.climbingData.balanced
                , "final state" ++ "," ++ statusToString model.climbingData.climbStatus
                , "defended?" ++ "," ++ boolToString model.climbingData.defended
                , "was defended?" ++ "," ++ boolToString model.climbingData.wasDefended
                , "shut down?" ++ "," ++ boolToString model.climbingData.shutDown
                , "comments" ++ "," ++ "'" ++ model.climbingData.comment ++ "'"
                ]
    in
    Download.string
        (String.concat [ String.join "-" <| teamDataGetter, ".csv" ])
        "content/text"
    <|
        String.join "\n"
            [ String.join "\n" <| teamDataGetter
            , autonomousGetter
            , teleopGetter
            , climbingGetter
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
            ( { model | askForClear = True }, dumpModel model )

        NoSubmit ->
            ( { model | pages = ClimbingPage, askForClear = False }, Cmd.none )

        Submit ->
            ( { model | pages = SubmitPage }, Cmd.none )

        SubmitDataMsg _ ->
            ( model, Cmd.none )

        ClearPages ->
            ( { model
                | autonomousData = Autonomous.init
                , teleopData = Teleop.init
                , climbingData = Climbing.init
                , pages = TeamDataPage
                , askForClear = False
                , teamData = currentMatch model.teamData
              }
            , Cmd.none
            )


teamDataToString : Model -> String
teamDataToString model =
    let
        rematchPrefix : String
        rematchPrefix =
            if model.teamData.isRematch && model.teamData.matchNumber /= "" then
                "R"

            else
                ""
    in
    rematchPrefix ++ (merge <| Result.map String.fromInt <| TeamData.getTeam model.teamData)


view : Model -> Element.Element Msg
view model =
    let
        page : String -> PagePosition -> List (Element.Attribute Msg) -> Element.Element Msg -> Element.Element Msg
        page name pagePosition attr msg =
            column
                [ centerX
                , width fill
                , Background.color <| Tuple.first <| findColor <| TeamData.stationToString model.teamData.station
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
                            , Font.glow Colors.black 3
                            , centerX
                            , height <| fillPortion 1
                            , Font.underline
                            , Element.paddingXY 0 10
                            ]
                    , text ("scouted team: " ++ teamDataToString model)
                        |> el
                            [ Font.size 47
                            , Font.color Colors.white
                            , Font.glow Colors.black 3
                            , Font.bold
                            , height <| fillPortion 1
                            , centerX
                            ]
                    ]
                , el attr <| msg
                , stylishPage pagePosition model.askForClear
                ]
    in
    case model.pages of
        TeamDataPage ->
            model.teamData
                |> TeamData.view
                |> Element.map TeamDataMsg
                |> page "Registration"
                    FirstPage
                    [ height fill
                    , width fill
                    , Background.color <| Tuple.second <| findColor <| TeamData.stationToString model.teamData.station
                    ]

        AutonomousPage ->
            model.autonomousData
                |> Autonomous.view
                |> Element.map AutonomousDataMsg
                |> page "Autonomous"
                    MiddlePage
                    [ Background.color <| Tuple.second <| findColor <| TeamData.stationToString model.teamData.station
                    , width fill
                    , Element.height <| Element.fillPortion 5
                    ]

        TeleopPage ->
            Teleop.view model.teleopData
                |> Element.map TeleopDataMsg
                |> page "Teleop"
                    MiddlePage
                    [ height fill
                    , width fill
                    , Background.color <| Tuple.second <| findColor <| TeamData.stationToString model.teamData.station
                    ]

        ClimbingPage ->
            Climbing.view model.climbingData
                |> Element.map ClimbingDataMsg
                |> page "End-game"
                    LastPage
                    [ height fill
                    , width fill
                    , Background.color <| Tuple.second <| findColor <| TeamData.stationToString model.teamData.station
                    ]

        SubmitPage ->
            page
                "Submit"
                SubmitPosPage
                [ centerX
                , centerY
                ]
                << Element.map SubmitDataMsg
            <|
                column [ Font.size 70 ]
                    [ el [ centerX ]
                        (text "Are you sure")
                    , el []
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
