module GetMatch exposing (getMatch, maybeIntToInt, unwrapToString)

import List.Extra exposing (getAt)
import Maybe.Extra exposing (unwrap)


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


checkMatch : Maybe Int -> Maybe Match
checkMatch match =
    case match of
        Nothing ->
            Nothing

        Just n ->
            getAt (n - 1) matches


unwrapToString : Maybe Int -> String
unwrapToString maybeInt =
    unwrap "" String.fromInt maybeInt


maybeIntToInt : Maybe Int -> Int
maybeIntToInt mi =
    case mi of
        Nothing ->
            0

        Just n ->
            n


getMatch : Maybe Int -> Maybe Int -> String
getMatch team match =
    stationIndex match <| maybeIntToInt team


stationIndex : Maybe Int -> Int -> String
stationIndex match team =
    case checkMatch match of
        Nothing ->
            "Not a match"

        Just matchData ->
            if matchData.blue.one == team then
                "Blue 1"

            else if matchData.blue.two == team then
                "Blue 2"

            else if matchData.blue.three == team then
                "Blue 3"

            else if matchData.red.one == team then
                "Red 1"

            else if matchData.red.two == team then
                "Red 2"

            else if matchData.red.three == team then
                "Red 3"

            else
                "Team not in this match"
