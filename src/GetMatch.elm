module GetMatch exposing (AllianceColor(..), AllianceStation, Match, StationNumber(..), getTeam, matches)


type alias Alliance =
    { one : Int
    , two : Int
    , three : Int
    }


type alias Match =
    { blue : Alliance
    , red : Alliance
    }


matches : List Match
matches =
    [ { blue = { one = 1690, two = 1574, three = 3339 }, red = { one = 254, two = 2056, three = 1323 } }
    , { blue = { one = 118, two = 1577, three = 1024 }, red = { one = 2056, two = 1690, three = 254 } }
    , { blue = { one = 1574, two = 3339, three = 1577 }, red = { one = 1323, two = 1024, three = 118 } }
    , { blue = { one = 3339, two = 1574, three = 1577 }, red = { one = 1024, two = 118, three = 2056 } }
    ]


type AllianceColor
    = Blue
    | Red


type StationNumber
    = One
    | Two
    | Three


type alias AllianceStation =
    ( AllianceColor, StationNumber )


getTeam : AllianceStation -> Match -> Int
getTeam chosenStation =
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
