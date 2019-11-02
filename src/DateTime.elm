module DateTime exposing (DateTime, addOneYear, fromList, isGreaterThan, toString)


type alias DateTime =
    { year : Int
    , month : Int
    , day : Int
    , hour : Int
    , min : Int
    }


fromList : List Int -> Maybe DateTime
fromList ints =
    case ints of
        [ year, month, day, hour, min ] ->
            Just <| DateTime year month day hour min

        _ ->
            Nothing


toString : DateTime -> String
toString { year, month, day, hour, min } =
    let
        date =
            List.map (String.fromInt >> appendZero) [ year, month, day ] |> String.join "-"

        time =
            List.map (String.fromInt >> appendZero) [ hour, min ] |> String.join ":"
    in
    String.join " " [ date, time ]


appendZero : String -> String
appendZero string =
    if String.length string == 1 then
        "0" ++ string

    else
        string


addOneYear : DateTime -> DateTime
addOneYear dateTime =
    { dateTime | year = dateTime.year + 1 }


isGreaterThan : DateTime -> DateTime -> Bool
isGreaterThan this other =
    (this.year > other.year)
        || (this.year == other.year && this.month > other.month)
        || (this.year == other.year && this.month == other.month && this.day > other.day)
        || (this.year
                == other.year
                && this.month
                == other.month
                && this.day
                == other.day
                && this.hour
                > other.hour
           )
        || (this.year
                == other.year
                && this.month
                == other.month
                && this.day
                == other.day
                && this.hour
                == other.hour
                && this.min
                > other.min
           )
