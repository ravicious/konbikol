module TicketParser exposing (parseStrings)

import Array exposing (Array)
import DateTime exposing (DateTime)
import List.Extra
import Maybe.Extra
import Ticket exposing (Ticket)


type alias RawTicketData =
    { departureStation : String
    , arrivalStation : String
    , departureDate : String
    , arrivalDate : String
    , departureTime : String
    , arrivalTime : String
    , train : String
    , carriage : String
    , seat : String
    , purchaseDateTime : String
    , travelClass : String
    }


type alias StringsContext =
    { importantStrings : Array String
    , allStrings : Array String
    }


parseStrings : Array String -> Result String Ticket
parseStrings strings =
    let
        stringsList =
            Array.toList strings

        {- The strings that follow "Informacje o podróży:" have presumably constant order and
           contain details that we need to construct a ticket.
        -}
        importantStrings =
            stringsList
                |> List.Extra.dropWhile (\string -> string /= "Informacje o podróży:")
                |> Array.fromList

        -- Purchase time helps us to determine what year the departure and the arrival are.
        indexOfPurchaseDateTime =
            stringsList
                |> List.Extra.findIndex (\string -> string == "Zapłacono i wystawiono dnia:")
                |> Maybe.map ((+) 1)
                |> Result.fromMaybe "No purchase time found (was looking for \"Zapłacono i wystawiono dnia:\" in the ticket)"

        indexOfTravelClass =
            stringsList
                |> List.Extra.findIndex (\string -> String.startsWith "PRZEZ:" string)
                |> Maybe.map (\i -> i - 2)
                |> Result.fromMaybe "No string starting with \"PRZEZ:\" found, which is needed for determining travel class"

        stringsContext =
            { importantStrings = importantStrings, allStrings = strings }

        {- Since we know the order of strings upfront, we extract the useful bits of
           information here and fail as soon as there's a problem with extracting any of them.
        -}
        rawTicketData =
            ( stringsContext, Ok RawTicketData )
                |> captureImportantStringsIndex 9
                |> captureImportantStringsIndex 10
                |> captureImportantStringsIndex 11
                |> captureImportantStringsIndex 12
                |> captureImportantStringsIndex 13
                |> captureImportantStringsIndex 14
                |> captureImportantStringsIndex 15
                |> captureImportantStringsIndex 16
                |> captureImportantStringsIndex 18
                |> captureUnsafeAllStringsIndex indexOfPurchaseDateTime
                |> captureUnsafeAllStringsIndex indexOfTravelClass
                |> Tuple.second
                |> Result.andThen cleanupPurchaseDateTime

        purchaseDateTime =
            rawTicketData
                |> Result.map .purchaseDateTime
                |> Result.andThen parsePurchaseDateTime

        departureDateTime =
            Result.map2 Tuple.pair rawTicketData purchaseDateTime
                |> Result.andThen
                    (\( rawTicketData_, purchaseDateTime_ ) ->
                        constructTicketDateTime
                            purchaseDateTime_
                            rawTicketData_.departureDate
                            rawTicketData_.departureTime
                    )

        arrivalDateTime =
            Result.map2 Tuple.pair rawTicketData purchaseDateTime
                |> Result.andThen
                    (\( rawTicketData_, purchaseDateTime_ ) ->
                        constructTicketDateTime
                            purchaseDateTime_
                            rawTicketData_.arrivalDate
                            rawTicketData_.arrivalTime
                    )
    in
    Result.map3 constructTicket rawTicketData departureDateTime arrivalDateTime


{-| Allows us to capture indexes from the strings array in a pipeline style. Inside the result
there's a function which composes a record, in this case RawTicketData.
-}
captureImportantStringsIndex : Int -> ( StringsContext, Result String (String -> a) ) -> ( StringsContext, Result String a )
captureImportantStringsIndex index ( { importantStrings } as stringsContext, result ) =
    ( stringsContext
    , Result.andThen
        (\recordFunction ->
            Array.get index importantStrings
                |> Result.fromMaybe ("importantStrings index out of bounds (" ++ String.fromInt index ++ ")")
                -- Spaces around strings are unimportant, but PKP sometimes adds them after
                -- stations.
                |> Result.map (String.trim >> recordFunction)
        )
        result
    )


{-| Captures content under an index. That index is usually calculated based on the index of a
specific string in the ticket. That's why we use Result for index in case that string wasn't found.
-}
captureUnsafeAllStringsIndex : Result String Int -> ( StringsContext, Result String (String -> a) ) -> ( StringsContext, Result String a )
captureUnsafeAllStringsIndex resultIndex ( { allStrings } as stringsContext, result ) =
    let
        updatedResult =
            Result.map2 Tuple.pair resultIndex result
                |> Result.andThen
                    (\( index, recordFunction ) ->
                        Array.get index allStrings
                            |> Result.fromMaybe
                                ("allStrings index out of bounds (" ++ String.fromInt index ++ ")")
                            -- Spaces around strings are unimportant, but PKP sometimes adds them
                            -- after stations.
                            |> Result.map (String.trim >> recordFunction)
                    )
    in
    ( stringsContext
    , updatedResult
    )


{-| The raw purchase date time string contains some irrelevant cruft that we don't need, for example
"2018-10-21 12:12:29(11111111)".
-}
cleanupPurchaseDateTime : RawTicketData -> Result String RawTicketData
cleanupPurchaseDateTime rawTicketData =
    rawTicketData.purchaseDateTime
        |> String.split "("
        |> List.head
        |> Result.fromMaybe ("Couldn't cleanup purchase date time: " ++ rawTicketData.purchaseDateTime)
        |> Result.map
            (\cleanPurchaseDateTime ->
                { rawTicketData | purchaseDateTime = cleanPurchaseDateTime }
            )


{-| Decomposes a string with the format of "2018-10-21 12:12:29" into DateTime.
-}
parsePurchaseDateTime : String -> Result String DateTime
parsePurchaseDateTime string =
    case String.split " " string of
        [ rawYyyymmdd, rawHhmmss ] ->
            let
                yyyymmdd =
                    splitStringAndParseInts "-" rawYyyymmdd

                -- We don't need seconds, hence taking only first two items.
                hhmm =
                    splitStringAndParseInts ":" rawHhmmss |> List.take 2
            in
            List.append yyyymmdd hhmm
                |> DateTime.fromList
                |> Result.fromMaybe
                    ("Some part of purchase date time failed to be cast to an integer: "
                        ++ rawYyyymmdd
                        ++ " "
                        ++ rawHhmmss
                    )

        _ ->
            Err <| "Couldn't split purchase date into date and time part: " ++ string


{-| date is dd.mm and time is hh:mm. Year is taken from the purchase date time.
-}
constructTicketDateTime : DateTime -> String -> String -> Result String DateTime
constructTicketDateTime purchaseDateTime date time =
    List.concat
        [ [ purchaseDateTime.year ]
        , splitStringAndParseInts "." date |> List.reverse
        , splitStringAndParseInts ":" time
        ]
        |> DateTime.fromList
        |> Result.fromMaybe ("Couldn't construct date time from " ++ date ++ " " ++ time)
        |> Result.map
            (\dateTime ->
                {- By default we grab the year from the purchase date time for the departure or
                   arrival date. However, if you buy a ticket on December 31st for January 2nd, we
                   should say that the departure will be on the next year.

                   You can't buy a ticket after the ride has started, so technically the purchase
                   date time should never be greater than the parsed departure or arrival date. If
                   that's the case, then it means that the departure or arrival will happen next
                   year and we should add one year to that date time.
                -}
                if DateTime.isGreaterThan purchaseDateTime dateTime then
                    DateTime.addOneYear dateTime

                else
                    dateTime
            )


constructTicket : RawTicketData -> DateTime -> DateTime -> Ticket
constructTicket rawTicketData departureDateTime arrivalDateTime =
    { departure = departureDateTime
    , arrival = arrivalDateTime
    , departureStation = rawTicketData.departureStation
    , arrivalStation = rawTicketData.arrivalStation
    , train = rawTicketData.train
    , carriage = rawTicketData.carriage
    , seat = rawTicketData.seat
    , travelClass = rawTicketData.travelClass
    }


{-| Splits a string by the given separator and then attempts to convert each resulting string to an
integer. If any of the strings fails the conversion, returns an empty list.
-}
splitStringAndParseInts : String -> String -> List Int
splitStringAndParseInts separator string =
    String.split separator string
        |> List.map String.toInt
        |> Maybe.Extra.combine
        |> Maybe.withDefault []
