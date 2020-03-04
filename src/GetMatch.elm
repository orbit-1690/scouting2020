module GetMatch exposing (AllianceColor(..), Match, StationNumber(..), TeamStation, getTeamNum, matches)

import Json.Decode as JD
import RemoteData


type alias Alliance =
    { one : Int
    , two : Int
    , three : Int
    }


type alias Match =
    { blue : Alliance
    , red : Alliance
    }


type alias StringMatch =
    { blueTeams : List String
    , redTeams : List String
    }


type alias Matches =
    List StringMatch


matches : List Match
matches =
    [ { blue = { one = 2212, two = 4590, three = 2230 }, red = { one = 5747, two = 1577, three = 5038 } }
    , { blue = { one = 1574, two = 3034, three = 6230 }, red = { one = 7079, two = 7845, three = 2096 } }
    , { blue = { one = 7112, two = 6738, three = 6736 }, red = { one = 7177, two = 7554, three = 1943 } }
    , { blue = { one = 3211, two = 5715, three = 4319 }, red = { one = 5135, two = 5990, three = 6740 } }
    , { blue = { one = 3083, two = 5554, three = 8223 }, red = { one = 4661, two = 4586, three = 8333 } }
    , { blue = { one = 5135, two = 1937, three = 2212 }, red = { one = 6230, two = 7554, three = 1580 } }
    , { blue = { one = 2096, two = 4661, three = 1577 }, red = { one = 4319, two = 6740, three = 4586 } }
    , { blue = { one = 7845, two = 7112, three = 5990 }, red = { one = 3083, two = 5715, three = 7079 } }
    , { blue = { one = 5554, two = 1580, three = 5747 }, red = { one = 6738, two = 3034, three = 5038 } }
    , { blue = { one = 1937, two = 8333, three = 6736 }, red = { one = 4590, two = 3211, three = 1574 } }
    , { blue = { one = 1943, two = 2230, three = 1580 }, red = { one = 1577, two = 8223, three = 7177 } }
    , { blue = { one = 2096, two = 4590, three = 6740 }, red = { one = 7079, two = 5554, three = 8333 } }
    , { blue = { one = 3034, two = 8223, three = 1943 }, red = { one = 1574, two = 5990, three = 4661 } }
    , { blue = { one = 5747, two = 6738, three = 5715 }, red = { one = 6736, two = 5135, three = 6230 } }
    , { blue = { one = 2212, two = 7845, three = 4586 }, red = { one = 1937, two = 5038, three = 7112 } }
    , { blue = { one = 7177, two = 2230, three = 4319 }, red = { one = 7554, two = 3083, three = 3211 } }
    , { blue = { one = 7845, two = 1580, three = 6740 }, red = { one = 4586, two = 7112, three = 5747 } }
    , { blue = { one = 4319, two = 7079, three = 6738 }, red = { one = 1577, two = 3034, three = 3211 } }
    , { blue = { one = 7554, two = 5990, three = 6736 }, red = { one = 2230, two = 5038, three = 8223 } }
    , { blue = { one = 2096, two = 3083, three = 8333 }, red = { one = 4590, two = 1943, three = 6230 } }
    , { blue = { one = 1574, two = 7177, three = 5135 }, red = { one = 2212, two = 4661, three = 5554 } }
    , { blue = { one = 5715, two = 6230, three = 7845 }, red = { one = 2230, two = 3211, three = 1937 } }
    , { blue = { one = 4586, two = 5554, three = 7554 }, red = { one = 3034, two = 5747, three = 4590 } }
    , { blue = { one = 6740, two = 7079, three = 6736 }, red = { one = 3083, two = 7112, three = 2212 } }
    , { blue = { one = 8223, two = 8333, three = 5990 }, red = { one = 1580, two = 6738, three = 4661 } }
    , { blue = { one = 5715, two = 5038, three = 7177 }, red = { one = 1943, two = 4319, three = 1574 } }
    , { blue = { one = 5135, two = 1577, three = 7112 }, red = { one = 2096, two = 1937, three = 5990 } }
    , { blue = { one = 6740, two = 1943, three = 2212 }, red = { one = 5038, two = 1574, three = 1580 } }
    , { blue = { one = 4661, two = 5715, three = 8223 }, red = { one = 6230, two = 7079, three = 1577 } }
    , { blue = { one = 3034, two = 4319, three = 1937 }, red = { one = 3083, two = 7177, three = 5747 } }
    , { blue = { one = 8333, two = 7554, three = 6738 }, red = { one = 5554, two = 3211, three = 7845 } }
    , { blue = { one = 4586, two = 6736, three = 4590 }, red = { one = 5135, two = 2096, three = 2230 } }
    , { blue = { one = 8223, two = 2212, three = 7554 }, red = { one = 3034, two = 6740, three = 3083 } }
    , { blue = { one = 1580, two = 2096, three = 7177 }, red = { one = 1577, two = 8333, three = 7845 } }
    , { blue = { one = 6230, two = 5747, three = 1937 }, red = { one = 6736, two = 4319, three = 4661 } }
    , { blue = { one = 4586, two = 1574, three = 2230 }, red = { one = 6738, two = 5135, three = 4590 } }
    , { blue = { one = 5554, two = 5990, three = 5038 }, red = { one = 7112, two = 1943, three = 3211 } }
    , { blue = { one = 7079, two = 5747, three = 1574 }, red = { one = 5715, two = 2096, three = 7554 } }
    , { blue = { one = 1937, two = 1577, three = 6738 }, red = { one = 8333, two = 6740, three = 5038 } }
    , { blue = { one = 5990, two = 3083, three = 1943 }, red = { one = 5715, two = 4586, three = 5135 } }
    , { blue = { one = 6230, two = 3211, three = 8223 }, red = { one = 7112, two = 4590, three = 5554 } }
    , { blue = { one = 4661, two = 7177, three = 3034 }, red = { one = 2230, two = 6736, three = 7845 } }
    , { blue = { one = 1580, two = 4319, three = 3083 }, red = { one = 2212, two = 7079, three = 3211 } }
    , { blue = { one = 7845, two = 5135, three = 3034 }, red = { one = 5747, two = 8223, three = 6740 } }
    , { blue = { one = 5038, two = 2096, three = 4586 }, red = { one = 6738, two = 7177, three = 6230 } }
    , { blue = { one = 8333, two = 2212, three = 1580 }, red = { one = 7112, two = 2230, three = 5715 } }
    , { blue = { one = 6736, two = 1574, three = 5554 }, red = { one = 5990, two = 1577, three = 4319 } }
    , { blue = { one = 1943, two = 4661, three = 7079 }, red = { one = 4590, two = 7554, three = 1937 } }
    , { blue = { one = 5038, two = 6230, three = 4319 }, red = { one = 8223, two = 1580, three = 5135 } }
    , { blue = { one = 7554, two = 6740, three = 4661 }, red = { one = 7079, two = 4586, three = 7177 } }
    , { blue = { one = 5554, two = 1937, three = 5715 }, red = { one = 6736, two = 3034, three = 2212 } }
    , { blue = { one = 8333, two = 1574, three = 7112 }, red = { one = 5990, two = 5747, three = 2230 } }
    , { blue = { one = 4590, two = 3083, three = 1577 }, red = { one = 6738, two = 7845, three = 1943 } }
    , { blue = { one = 3211, two = 4586, three = 5990 }, red = { one = 2096, two = 2212, three = 1574 } }
    , { blue = { one = 7177, two = 7845, three = 4590 }, red = { one = 1580, two = 6736, three = 5715 } }
    , { blue = { one = 7079, two = 7112, three = 3034 }, red = { one = 4319, two = 5135, three = 8333 } }
    , { blue = { one = 3211, two = 6738, three = 2096 }, red = { one = 4661, two = 5038, three = 3083 } }
    , { blue = { one = 7554, two = 2230, three = 1577 }, red = { one = 6740, two = 6230, three = 5554 } }
    , { blue = { one = 5747, two = 1943, three = 2096 }, red = { one = 8223, two = 1937, three = 7079 } }
    ]


type AllianceColor
    = Blue
    | Red


type StationNumber
    = One
    | Two
    | Three


type alias TeamStation =
    ( AllianceColor, StationNumber )


matchesParser : JD.Decoder Matches
matchesParser =
    JD.map2 StringMatch
        (JD.at [ "alliances", "blue", "team_keys" ] (JD.list JD.string))
        (JD.at [ "alliances", "red", "team_keys" ] (JD.list JD.string))
        |> JD.list


getTeamNum : TeamStation -> Match -> Int
getTeamNum chosenStation =
    let
        alliance : Match -> Alliance
        alliance =
            case chosenStation of
                ( Red, _ ) ->
                    .red

                ( Blue, _ ) ->
                    .blue

        number : Alliance -> Int
        number =
            case chosenStation of
                ( _, One ) ->
                    .one

                ( _, Two ) ->
                    .two

                ( _, Three ) ->
                    .three
    in
    number << alliance
