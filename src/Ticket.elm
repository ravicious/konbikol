module Ticket exposing (Ticket)

import DateTime exposing (DateTime)


type alias Ticket =
    { departure : DateTime
    , arrival : DateTime
    , departureStation : String
    , arrivalStation : String
    , train : String
    , carriage : String
    , seat : String
    }
