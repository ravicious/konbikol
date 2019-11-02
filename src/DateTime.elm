module DateTime exposing (DateTime, fromList)


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
