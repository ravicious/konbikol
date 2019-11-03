port module Main exposing (main)

import Array exposing (Array)
import Browser
import CalendarEvent exposing (CalendarEvent)
import DateTime
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode
import Ticket exposing (Ticket)
import TicketParser


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { ticket : TicketStatus }


type TicketStatus
    = NotUploaded
    | Parsing
    | Success Ticket
    | ParseError String


type Msg
    = GotFile Decode.Value
    | GotPdfStrings (Array String)
    | DownloadTicket Ticket


port parsePdf : Encode.Value -> Cmd msg


port downloadEvent : CalendarEvent -> Cmd msg


port extractedTextFromPdf : (Array String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions _ =
    extractedTextFromPdf GotPdfStrings


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ticket = NotUploaded }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile file ->
            ( { model | ticket = Parsing }, parsePdf file )

        GotPdfStrings strings ->
            ( case TicketParser.parseStrings strings of
                Ok ticket ->
                    { model | ticket = Success ticket }

                Err error ->
                    { model | ticket = ParseError error }
            , Cmd.none
            )

        DownloadTicket ticket ->
            ( model, downloadEvent <| Ticket.toCalendarEvent ticket )


fileDecoder : Decode.Decoder Decode.Value
fileDecoder =
    Decode.at [ "target", "files" ] (Decode.list Decode.value)
        |> Decode.andThen
            (\files ->
                case List.head files of
                    Just file ->
                        Decode.succeed file

                    Nothing ->
                        Decode.fail "No files found under target.files of change event of an input[type=\"file\"]"
            )


view : Model -> Html Msg
view model =
    case model.ticket of
        NotUploaded ->
            article []
                [ p [] [ text "Please select a PDF file with the ticket." ]
                , input
                    [ type_ "file"
                    , accept "application/pdf"
                    , on "change" (Decode.map GotFile fileDecoder)
                    ]
                    []
                ]

        Parsing ->
            text "Parsing ticket"

        Success ticket ->
            viewTicket ticket

        ParseError message ->
            text <| "Something went wrong: " ++ message


viewTicket : Ticket -> Html Msg
viewTicket ticket =
    article []
        [ p [] [ text <| ticket.departureStation ++ " → " ++ ticket.arrivalStation ]
        , p []
            [ text <| DateTime.toString ticket.departure ++ " → " ++ DateTime.toString ticket.arrival ]
        , p [] [ text <| "🚂 " ++ ticket.train ]
        , p [] [ text <| "🚃 " ++ ticket.carriage ++ " 💺 " ++ ticket.seat ]
        , button
            [ style "font-weight" "bold"
            , onClick (DownloadTicket ticket)
            ]
            [ text "Add event to calendar" ]
        ]
