module CalendarEvent exposing (CalendarEvent)

import DateTime exposing (DateTime)


type alias CalendarEvent =
    { subject : String
    , description : String
    , location : String
    , start : DateTime
    , end : DateTime

    {- Each calendar event needs to have a unique ID. Otherwise calendar apps would treat all events
       created through the app as the same event and update an existing event with the same
       ID rather than add a new event to the calendar.
    -}
    , uid : String
    }
