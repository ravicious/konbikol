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
    , carriage : String
    , seat : String
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
                ++ ticket.carriage
                ++ ticket.seat
                |> Murmur3.hashString 1
                |> String.fromInt
    in
    { subject = ticket.departureStation ++ " → " ++ ticket.arrivalStation

    -- ics.js doesn't handle newlines in event description, so we have to escape that \n.
    , description = "🚂 " ++ ticket.train ++ "\\n🚃 " ++ ticket.carriage ++ " 💺 " ++ ticket.seat
    , location = ""
    , start = ticket.departure
    , end = ticket.arrival
    , uid = uid
    }
