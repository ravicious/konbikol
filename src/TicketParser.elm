module TicketParser exposing (parseStrings)

import Array exposing (Array)
import Ticket exposing (Ticket)
import TicketParser.Mid2021Format as Mid2021Format
import TicketParser.OldFormat as OldFormat


type TicketFormat
    = Mid2021
    | Old


parseStrings : Array String -> Result String Ticket
parseStrings strings =
    case detectTicketFormat strings of
        Just Mid2021 ->
            Mid2021Format.parseStrings strings

        Just Old ->
            OldFormat.parseStrings strings

        _ ->
            Err "Unknown ticket format"


detectTicketFormat : Array String -> Maybe TicketFormat
detectTicketFormat strings =
    detectTicketFormatHelp strings 0


detectTicketFormatHelp : Array String -> Int -> Maybe TicketFormat
detectTicketFormatHelp strings index =
    case Array.get index strings of
        Nothing ->
            Nothing

        Just string ->
            case string of
                " to Twój plan podróży:" ->
                    Just Mid2021

                "BILET INTERNETOWY" ->
                    Just Old

                _ ->
                    detectTicketFormatHelp strings (index + 1)
