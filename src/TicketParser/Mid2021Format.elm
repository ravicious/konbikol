module TicketParser.Mid2021Format exposing (parseStrings)

import Array exposing (Array)
import DateTime exposing (DateTime)
import List.Extra
import Maybe.Extra
import Result.Extra exposing (andMap)
import Ticket exposing (Ticket)


departureStationIndex =
    1


arrivalStationIndex =
    2


parseStrings : Array String -> Result String Ticket
parseStrings strings =
    let
        departureStationResult =
            Array.get departureStationIndex strings
                |> Maybe.map String.trim
                |> Result.fromMaybe "Couldn't find the departure station"

        arrivalStationResult =
            Array.get arrivalStationIndex strings
                |> Maybe.map String.trim
                |> Result.fromMaybe "Couldn't find the arrival station"

        secondArrivalStationOccurenceIndexResult =
            arrivalStationResult
                |> Result.andThen
                    (\arrivalStation ->
                        List.Extra.findIndex ((==) arrivalStation) (Array.toList strings)
                            |> Result.fromMaybe "Couldn't find the second occurence of the arrival station"
                    )
    in
    Ok makeTriple
        |> andMap departureStationResult
        |> andMap arrivalStationResult
        |> andMap secondArrivalStationOccurenceIndexResult
        |> Result.andThen
            (\( departureStation, arrivalStation, secondArrivalStationOccurenceIndex ) ->
                -- The arrival station is mentioned for the second time inside a table with all the
                -- ticket details. The exact index may vary, but once we find the second occurence
                -- of the arrival station, the rest of the data should be in a predictable position.
                case
                    Array.toList <|
                        Array.slice (secondArrivalStationOccurenceIndex + 1)
                            (secondArrivalStationOccurenceIndex + 13)
                            strings
                of
                    [ departureTime, arrivalTimeRaw, dateRaw, _, _, train1, train2, travelClass, carriageNumber1, carriageNumber2, seat1, seat2 ] ->
                        let
                            date =
                                splitStringAndParseInts "." dateRaw
                                    -- dateRaw looks like this: `20.07.2021`.
                                    -- Reverse it so that it follows the yyyy mm dd format that
                                    -- `DateTime.fromList` expects.
                                    |> List.reverse

                            departureDateTimeResult =
                                List.append date (splitStringAndParseInts ":" departureTime)
                                    |> DateTime.fromList
                                    |> Result.fromMaybe "Couldn't determine departure time"

                            arrivalDateTimeResult =
                                List.append date (arrivalTimeRaw |> String.right 5 |> splitStringAndParseInts ":")
                                    |> DateTime.fromList
                                    |> Result.fromMaybe "Couldn't determine departure time"
                        in
                        Result.map2
                            (\departureDateTime arrivalDateTime ->
                                let
                                    train =
                                        train1 ++ " " ++ train2

                                    carriageNumber =
                                        carriageNumber1 ++ carriageNumber2

                                    seat =
                                        seat1 ++ seat2
                                in
                                Ticket departureDateTime
                                    arrivalDateTime
                                    departureStation
                                    arrivalStation
                                    train
                                    carriageNumber
                                    -- This ticket format shows the carriage type within what's in
                                    -- `carriageNumber`, so we pass an empty string as `carriageType`.
                                    ""
                                    seat
                                    travelClass
                            )
                            departureDateTimeResult
                            arrivalDateTimeResult

                    slice ->
                        Err "Data in the travel information table has different format than expected"
            )


{-| Splits a string by the given separator and then attempts to convert each resulting string to an
integer. If any of the strings fails the conversion, returns an empty list.
-}
splitStringAndParseInts : String -> String -> List Int
splitStringAndParseInts separator string =
    String.split separator string
        |> List.map String.toInt
        |> Maybe.Extra.combine
        |> Maybe.withDefault []


makeTriple a b c =
    ( a, b, c )
