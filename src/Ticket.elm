module Ticket exposing (Ticket, toCalendarEvent)

import CalendarEvent exposing (CalendarEvent)
import DateTime exposing (DateTime)
import Murmur3


type alias Ticket =
    { departure : DateTime
    , arrival : DateTime
    , departureStation : String
    , arrivalStation : String
    , train : String
    , carriageNumber : String
    , carriageType : String
    , seat : String
    , travelClass : String
    }


toCalendarEvent : Ticket -> CalendarEvent
toCalendarEvent ticket =
    let
        uid =
            ticket.departureStation
                ++ ticket.arrivalStation
                ++ DateTime.toString ticket.departure
                ++ DateTime.toString ticket.arrival
                ++ ticket.train
                ++ ticket.carriageNumber
                ++ ticket.seat
                |> Murmur3.hashString 1
                |> String.fromInt

        subject =
            ticket.departureStation ++ " → " ++ ticket.arrivalStation

        trainCarriageNumberAndSeat =
            "🚂 " ++ ticket.train ++ "\n🚃 " ++ ticket.carriageNumber ++ " 💺 " ++ ticket.seat

        travelClassAndCarriageType =
            "klasa " ++ ticket.travelClass
                ++ (if String.isEmpty ticket.carriageType then
                        ""

                    else
                        ", " ++ ticket.carriageType
                   )

        description =
            (trainCarriageNumberAndSeat ++ "\n" ++ travelClassAndCarriageType)
                -- ics.js doesn't handle newlines in event description, so we have to escape them.
                |> String.replace "\n" "\\n"
    in
    { subject = subject
    , description = description
    , location = ""
    , start = ticket.departure
    , end = ticket.arrival
    , uid = uid
    }
