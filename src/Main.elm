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
import Time


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { ticket : TicketStatus
    , placeholderCharacter : String
    }


type TicketStatus
    = NotUploaded
    | Parsing
    | Success Ticket
    | ParseError String


type Msg
    = GotFile Decode.Value
    | GotPdfStrings (Array String)
    | DownloadTicket Ticket
    | ResetApp
    | SetPlaceholderCharacter String


port parsePdf : Encode.Value -> Cmd msg


port downloadEvent : CalendarEvent -> Cmd msg


port extractedTextFromPdf : (Array String -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        togglePlaceholderSub =
            case model.ticket of
                Parsing ->
                    Time.every 500 (\_ -> togglePlaceholderCharacter model.placeholderCharacter)

                _ ->
                    Sub.none
    in
    Sub.batch
        [ extractedTextFromPdf GotPdfStrings
        , togglePlaceholderSub
        ]


character1 =
    "░"


character2 =
    "▒"


togglePlaceholderCharacter : String -> Msg
togglePlaceholderCharacter char =
    let
        newChar =
            if char == character1 then
                character2

            else
                character1
    in
    SetPlaceholderCharacter newChar


init : () -> ( Model, Cmd Msg )
init _ =
    ( { ticket = NotUploaded
      , placeholderCharacter = character1
      }
    , Cmd.none
    )


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

        ResetApp ->
            init ()

        SetPlaceholderCharacter character ->
            ( { model | placeholderCharacter = character }, Cmd.none )


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
                , p [] [ text "The ticket isn't sent to any server, all processing is done on your device." ]
                , input
                    [ type_ "file"
                    , accept "application/pdf"
                    , on "change" (Decode.map GotFile fileDecoder)
                    ]
                    []
                , p []
                    [ a [ href "https://github.com/ravicious/konbikol" ] [ text "Source code" ]
                    ]
                ]

        Parsing ->
            viewTicketPlaceholder model.placeholderCharacter

        Success ticket ->
            viewTicket ticket

        ParseError message ->
            div []
                [ p [] [ text <| "Something went wrong: " ++ message ]
                , button [ onClick ResetApp ] [ text "Select another ticket" ]
                ]


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
        , button [ onClick ResetApp ] [ text "Select another ticket" ]
        ]


viewTicketPlaceholder : String -> Html Msg
viewTicketPlaceholder character =
    let
        placeholder =
            createPlaceholder character
    in
    article []
        [ p [] [ text <| placeholder 6 ++ " → " ++ placeholder 6 ]
        , p [] [ text <| placeholder 10 ++ " → " ++ placeholder 10 ]
        , p [] [ text <| "🚂 " ++ placeholder 6 ]
        , p [] [ text <| "🚃 " ++ placeholder 1 ++ " 💺 " ++ placeholder 2 ]
        , p [] [ text "Processing the ticket" ]
        ]


createPlaceholder : String -> Int -> String
createPlaceholder s n =
    String.repeat n s
