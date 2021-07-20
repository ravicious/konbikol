module TicketParser.OldFormat exposing (parseStrings)

import Array exposing (Array)
import DateTime exposing (DateTime)
import List.Extra
import Maybe.Extra
import String.Extra
import Ticket exposing (Ticket)


type alias RawTicketData =
    { departureStation : String
    , arrivalStation : String
    , departureDate : String
    , arrivalDate : String
    , departureTime : String
    , arrivalTime : String
    , train : String
    , carriageNumber : String
    , carriageType : String
    , seat : String
    , purchaseDateTime : String
    , travelClass : String
    }


type alias Indices =
    { travelDetailsSectionIndex : Int
    , purchaseDateTimeIndex : Int
    , travelClassIndex : Int
    }


parseStrings : Array String -> Result String Ticket
parseStrings strings =
    let
        stringsList =
            Array.toList strings

        -- The section that follows "Informacje o podróży:" has presumably constant order and
        -- contains most of the details that we need to construct a ticket.
        indexOfTravelDetailsSection =
            stringsList
                |> List.Extra.findIndex (\string -> string == "Informacje o podróży:")
                |> Result.fromMaybe "Failed to find the section with travel details (was looking for \"Informacje o podróży:\" in the ticket)"

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

        -- Since we know the order of the strings upfront, we extract the useful bits of information
        -- here and fail as soon as there's a problem with extracting any of them.
        rawTicketData : Result String RawTicketData
        rawTicketData =
            Result.map3 Indices indexOfTravelDetailsSection indexOfPurchaseDateTime indexOfTravelClass
                |> Result.andThen
                    (\{ travelDetailsSectionIndex, purchaseDateTimeIndex, travelClassIndex } ->
                        let
                            captureString =
                                captureStringAtIndex strings

                            captureTravelDetailsString =
                                (+) travelDetailsSectionIndex >> captureStringAtIndex strings
                        in
                        Ok RawTicketData
                            |> captureTravelDetailsString 9
                            |> captureTravelDetailsString 10
                            |> captureTravelDetailsString 11
                            |> captureTravelDetailsString 12
                            |> captureTravelDetailsString 13
                            |> captureTravelDetailsString 14
                            |> captureTravelDetailsString 15
                            |> captureTravelDetailsString 16
                            |> captureTravelDetailsString 19
                            |> captureTravelDetailsString 18
                            |> captureString purchaseDateTimeIndex
                            |> captureString travelClassIndex
                    )
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


{-| This function allows us to pipe a record function wrapped in a Result.
-}
captureStringAtIndex : Array String -> Int -> Result String (String -> a) -> Result String a
captureStringAtIndex strings index result =
    Array.get index strings
        |> Result.fromMaybe ("Index out of bounds (" ++ String.fromInt index ++ ")")
        -- Spaces around strings are unimportant, but PKP sometimes adds them
        -- after stations.
        |> Result.map2 (\recordFunction string -> String.trim string |> recordFunction) result


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
    , carriageNumber = rawTicketData.carriageNumber
    , carriageType =
        -- "1 m. do siedzenia; wagon bez przedziałów" -> "wagon bez przedziałów"
        rawTicketData.carriageType |> String.Extra.rightOf "; "
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
