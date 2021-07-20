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
    , hasHover : Bool
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
    | DragEnter
    | DragLeave
    | GotPdfjsError PdfjsError


type alias PdfjsError =
    { error : String, fileName : String }


port parsePdf : Encode.Value -> Cmd msg


port downloadEvent : CalendarEvent -> Cmd msg


port extractedTextFromPdf : (Array String -> msg) -> Sub msg


port pdfjsErrors : (PdfjsError -> msg) -> Sub msg


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
        , pdfjsErrors GotPdfjsError
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
      , hasHover = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile file ->
            ( { model | ticket = Parsing, hasHover = False }, parsePdf file )

        GotPdfjsError { error, fileName } ->
            let
                errorMessage =
                    if error == "InvalidPDFException" then
                        "the given file (" ++ fileName ++ ") doesn't appear to be a valid PDF file"

                    else
                        error
            in
            ( { model | ticket = ParseError errorMessage }, Cmd.none )

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

        DragEnter ->
            ( { model | hasHover = True }, Cmd.none )

        DragLeave ->
            ( { model | hasHover = False }, Cmd.none )


fileDecoder : Decode.Decoder Msg
fileDecoder =
    let
        keepFirstFile =
            \firstFile rest -> firstFile

        decodeFirstFileAt =
            \path -> Decode.at path (Decode.oneOrMore keepFirstFile Decode.value)
    in
    Decode.oneOf
        [ decodeFirstFileAt [ "target", "files" ] -- input[type=file]
        , decodeFirstFileAt [ "dataTransfer", "files" ] -- drag and drop
        ]
        |> Decode.map GotFile


view : Model -> Html Msg
view model =
    case model.ticket of
        NotUploaded ->
            layoutWithDragAndDrop model <|
                [ p [] [ text "konbikol converts PKP Intercity tickets to iCalendar events." ]
                , label [ class "file-input", for "pdf-file" ]
                    [ text "Please select a PDF file with the ticket or drop it anywhere on the page."
                    ]
                , p []
                    [ input
                        [ type_ "file"
                        , accept "application/pdf"
                        , on "change" fileDecoder
                        , id "pdf-file"
                        ]
                        []
                    ]
                , p [] [ text "The ticket isn't sent to any server, all processing is done on your device." ]
                , p []
                    [ a [ href "https://github.com/ravicious/konbikol" ] [ text "Source code" ]
                    ]
                ]

        Parsing ->
            layout <| viewParsingScreen model.placeholderCharacter

        Success ticket ->
            layoutWithDragAndDrop model <|
                viewTicket ticket

        ParseError message ->
            layoutWithDragAndDrop model <|
                [ p [] [ text <| "Something went wrong: " ++ message ]
                , selectTicketInput
                ]


layout : List (Html Msg) -> Html Msg
layout =
    article []


layoutWithDragAndDrop : Model -> List (Html Msg) -> Html Msg
layoutWithDragAndDrop model =
    article
        [ hijackOn "dragenter" (Decode.succeed DragEnter)
        , hijackOn "dragover" (Decode.succeed DragEnter)
        , hijackOn "dragleave" (Decode.succeed DragLeave)
        , hijackOn "drop" fileDecoder
        , class
            (if model.hasHover then
                "has-hover"

             else
                ""
            )
        ]


viewTicket : Ticket -> List (Html Msg)
viewTicket ticket =
    let
        ticketDetails =
            viewTicketDetails (Just ticket) ""
    in
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
        [ text "Select another ticket or drop it anywhere on the page"
        , input
            [ type_ "file"
            , accept "application/pdf"
            , on "change" fileDecoder
            ]
            []
        ]


viewParsingScreen : String -> List (Html Msg)
viewParsingScreen placeholderCharacter =
    let
        ticketDetails =
            viewTicketDetails Nothing placeholderCharacter
    in
    ticketDetails
        ++ [ p [] [ text "Processing the ticket" ]
           ]


viewTicketDetails : Maybe Ticket -> String -> List (Html Msg)
viewTicketDetails maybeTicket placeholderCharacter =
    let
        field =
            viewField maybeTicket placeholderCharacter

        fieldWithoutPlaceholder =
            viewField maybeTicket "" 0
    in
    [ p [] [ text <| field 6 .departureStation ++ " → " ++ field 6 .arrivalStation ]
    , time [] [ text <| field 10 (.departure >> DateTime.toString) ]
    , time [] [ text <| field 10 (.arrival >> DateTime.toString) ]
    , p [] [ text <| "🚂 " ++ field 6 .train ]
    , p [] [ text <| "🚃 " ++ field 2 .carriageNumber ++ " 💺 " ++ field 2 .seat ]
    , p [ class "details" ] [ text <| "klasa " ++ field 1 .travelClass ]
    , p [ class "details" ] [ text <| fieldWithoutPlaceholder .carriageType ]
    ]


viewField : Maybe Ticket -> String -> Int -> (Ticket -> String) -> String
viewField maybeTicket placeholderCharacter placeholderLength getFieldToText =
    maybeTicket
        |> Maybe.map getFieldToText
        |> Maybe.withDefault (String.repeat placeholderLength placeholderCharacter)


hijackOn : String -> Decode.Decoder msg -> Attribute msg
hijackOn event decoder =
    preventDefaultOn event (Decode.map hijack decoder)


hijack : msg -> ( msg, Bool )
hijack msg =
    ( msg, True )
