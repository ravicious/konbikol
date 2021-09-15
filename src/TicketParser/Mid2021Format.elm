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


type alias PreliminaryData =
    { departureStation : String
    , arrivalStation : String
    , secondArrivalStationOccurenceIndex : Int
    , legendIndex : Int
    }


parseStrings : Array String -> Result String Ticket
parseStrings strings =
    let
        stringsList =
            Array.toList strings

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
                        List.Extra.findIndex ((==) arrivalStation) stringsList
                            |> Result.fromMaybe "Couldn't find the second occurence of the arrival station"
                    )

        -- Used for collecting all seat info, described in detail later.
        legendIndexResult =
            List.Extra.findIndex (String.startsWith "LEGENDA: ") stringsList
                |> Result.fromMaybe "Couldn't find the legend under the travel details"
    in
    Ok PreliminaryData
        |> andMap departureStationResult
        |> andMap arrivalStationResult
        |> andMap secondArrivalStationOccurenceIndexResult
        |> andMap legendIndexResult
        |> Result.andThen
            (\{ departureStation, arrivalStation, secondArrivalStationOccurenceIndex, legendIndex } ->
                -- The arrival station is mentioned for the second time inside a table with all the
                -- ticket details. The exact index may vary, but once we find the second occurence
                -- of the arrival station, the rest of the data should be in a predictable position.
                case
                    Array.toList <|
                        Array.slice (secondArrivalStationOccurenceIndex + 1)
                            (secondArrivalStationOccurenceIndex + 11)
                            strings
                of
                    [ departureTime, arrivalTimeRaw, dateRaw, _, _, train1, train2, travelClass, carriageNumber1, carriageNumber2 ] ->
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

                            seat =
                                -- The list on which we case ends with carriageNumber2. What follows
                                -- after that up until the legend are seat numbers which we need to
                                -- collect into a single string.
                                strings
                                    |> Array.slice (secondArrivalStationOccurenceIndex + 11) legendIndex
                                    |> Array.toList
                                    |> String.join ""
                        in
                        Result.map2
                            (\departureDateTime arrivalDateTime ->
                                let
                                    train =
                                        train1 ++ " " ++ train2

                                    carriageNumber =
                                        carriageNumber1 ++ carriageNumber2
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


startsWithDigit : String -> Bool
startsWithDigit =
    String.uncons >> Maybe.map (Tuple.first >> Char.isDigit) >> Maybe.withDefault False
