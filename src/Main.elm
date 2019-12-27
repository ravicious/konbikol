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
                [ p [] [ text "konbikol converts PKP Intercity tickets to iCalendar events." ]
                , label [ class "file-input" ]
                    [ text "Please select a PDF file with the ticket."
                    , input
                        [ type_ "file"
                        , accept "application/pdf"
                        , on "change" (Decode.map GotFile fileDecoder)
                        ]
                        []
                    ]
                , p [] [ text "The ticket isn't sent to any server, all processing is done on your device." ]
                , p []
                    [ a [ href "https://github.com/ravicious/konbikol" ] [ text "Source code" ]
                    ]
                ]

        Parsing ->
            viewParsingScreen model.placeholderCharacter

        Success ticket ->
            viewTicket ticket

        ParseError message ->
            div []
                [ p [] [ text <| "Something went wrong: " ++ message ]
                , selectTicketInput
                ]


viewTicket : Ticket -> Html Msg
viewTicket ticket =
    let
        ticketDetails =
            viewTicketDetails (Just ticket) ""
    in
    article [] <|
        ticketDetails
            ++ [ button
                    [ style "font-weight" "bold", onClick (DownloadTicket ticket) ]
                    [ text "Add event to calendar" ]
               , br [] []
               , selectTicketInput
               ]


selectTicketInput : Html Msg
selectTicketInput =
    label [ class "file-input file-input--secondary" ]
        [ text "Select another ticket"
        , input
            [ type_ "file"
            , accept "application/pdf"
            , on "change" (Decode.map GotFile fileDecoder)
            ]
            []
        ]


viewParsingScreen : String -> Html Msg
viewParsingScreen placeholderCharacter =
    let
        ticketDetails =
            viewTicketDetails Nothing placeholderCharacter
    in
    article [] <|
        ticketDetails
            ++ [ p [] [ text "Processing the ticket" ]
               ]


viewTicketDetails : Maybe Ticket -> String -> List (Html Msg)
viewTicketDetails maybeTicket placeholderCharacter =
    let
        field =
            viewField maybeTicket placeholderCharacter
    in
    [ p [] [ text <| field 6 .departureStation ++ " → " ++ field 6 .arrivalStation ]
    , time [] [ text <| field 10 (.departure >> DateTime.toString) ]
    , time [] [ text <| field 10 (.arrival >> DateTime.toString) ]
    , p [] [ text <| "🚂 " ++ field 6 .train ]
    , p [] [ text <| "🚃 " ++ field 1 .carriageNumber ++ " 💺 " ++ field 2 .seat ]
    , p [ class "details" ] [ text <| "klasa " ++ field 1 .travelClass ++ ", " ++ field 12 .carriageType ]
    ]


viewField : Maybe Ticket -> String -> Int -> (Ticket -> String) -> String
viewField maybeTicket placeholderCharacter placeholderLength getFieldToText =
    maybeTicket
        |> Maybe.map getFieldToText
        |> Maybe.withDefault (String.repeat placeholderLength placeholderCharacter)
