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
import List.Nonempty as Nonempty exposing (Nonempty)
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
    { pageStatus : Status
    , placeholderCharacter : String
    , hasHover : Bool
    }


type Status
    = AwaitingUpload
    | ShowingTickets (Nonempty TicketStatus)


type TicketStatus
    = Parsing
    | Success Ticket
    | Failure ParseError


type alias ParseError =
    { fileName : Maybe String
    , message : String
    }


isParsed : TicketStatus -> Bool
isParsed status =
    case status of
        Parsing ->
            False

        _ ->
            True


type alias Index =
    Int


type Msg
    = GotFiles (Nonempty Decode.Value)
    | GotPdfStrings ( Index, String, Array String )
    | DownloadTicket Ticket
    | SetPlaceholderCharacter String
    | DragEnter
    | DragLeave
    | GotPdfjsError ( Index, ParseError )


port parsePdf : ( Index, Encode.Value ) -> Cmd msg


port downloadEvent : CalendarEvent -> Cmd msg


port extractedTextFromPdf : (( Index, String, Array String ) -> msg) -> Sub msg


port pdfjsErrors : (( Index, ParseError ) -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        togglePlaceholderSub =
            case model.pageStatus of
                ShowingTickets tickets ->
                    if Nonempty.any (not << isParsed) tickets then
                        Time.every 500 (\_ -> togglePlaceholderCharacter model.placeholderCharacter)

                    else
                        Sub.none

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
    ( { pageStatus = AwaitingUpload
      , placeholderCharacter = character1
      , hasHover = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFiles files ->
            let
                validatedFiles =
                    Nonempty.map validateMimeType files

                ticketStatuses =
                    Nonempty.map
                        (\validationResult ->
                            case validationResult of
                                Ok _ ->
                                    Parsing

                                Err parseError ->
                                    Failure parseError
                        )
                        validatedFiles

                cmds =
                    Cmd.batch <|
                        Nonempty.toList <|
                            Nonempty.indexedMap
                                (\index validationResult ->
                                    case validationResult of
                                        Ok file ->
                                            parsePdf ( index, file )

                                        Err _ ->
                                            Cmd.none
                                )
                                validatedFiles
            in
            ( { model
                | pageStatus = ShowingTickets ticketStatuses
                , hasHover = False
              }
            , cmds
            )

        GotPdfjsError ( index, { fileName, message } ) ->
            ( updateTicket index (Failure <| ParseError fileName message) model, Cmd.none )

        GotPdfStrings ( index, fileName, strings ) ->
            let
                ticketStatus =
                    case TicketParser.parseStrings strings of
                        Ok ticket ->
                            Success ticket

                        Err error ->
                            Failure <| ParseError (Just fileName) error
            in
            ( updateTicket index ticketStatus model
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


updateTicket : Index -> TicketStatus -> Model -> Model
updateTicket targetIndex newValue model =
    { model
        | pageStatus =
            case model.pageStatus of
                ShowingTickets tickets ->
                    ShowingTickets <|
                        Nonempty.indexedMap
                            (\index ticket ->
                                if index == targetIndex then
                                    newValue

                                else
                                    ticket
                            )
                            tickets

                _ ->
                    model.pageStatus
    }


validateMimeType : Decode.Value -> Result ParseError Decode.Value
validateMimeType rawFile =
    case Decode.decodeValue File.decoder rawFile of
        Ok file ->
            if File.mime file == "application/pdf" then
                Ok rawFile

            else
                Err <| ParseError (Just <| File.name file) "Not a PDF file"

        Err error ->
            Err <| ParseError Nothing (Decode.errorToString error)


filesDecoder : Decode.Decoder Msg
filesDecoder =
    let
        toNonempty =
            \firstFile rest -> Nonempty.singleton firstFile |> Nonempty.replaceTail rest

        decodeFilesAt =
            \path -> Decode.at path (Decode.oneOrMore toNonempty Decode.value)
    in
    Decode.oneOf
        [ decodeFilesAt [ "target", "files" ] -- input[type=file]
        , decodeFilesAt [ "dataTransfer", "files" ] -- drag and drop
        ]
        |> Decode.map GotFiles


view : Model -> Html Msg
view model =
    case model.pageStatus of
        AwaitingUpload ->
            layoutWithDragAndDrop model <|
                [ p [] [ text "konbikol converts PKP Intercity tickets to iCalendar events." ]
                , label [ class "file-input", for "pdf-file" ]
                    [ text "Please select PDF files with the tickets or drop them anywhere on the page."
                    ]
                , p []
                    [ input
                        [ type_ "file"
                        , accept "application/pdf"
                        , on "change" filesDecoder
                        , id "pdf-file"
                        , multiple True
                        ]
                        []
                    ]
                , p [] [ text "The tickets are not sent to any server, all processing is done on your device." ]
                , p []
                    [ a [ href "https://github.com/ravicious/konbikol" ] [ text "Source code" ]
                    ]
                ]

        ShowingTickets tickets ->
            let
                allTicketsAreParsed =
                    Nonempty.all isParsed tickets

                layoutFunction =
                    if allTicketsAreParsed then
                        layoutWithDragAndDrop model

                    else
                        layout

                topBar =
                    div
                        []
                        [ if allTicketsAreParsed then
                            selectTicketInput

                          else
                            text "Processing the tickets"
                        ]
            in
            layoutFunction
                (topBar
                    :: (Nonempty.toList tickets
                            |> List.map
                                (\ticketStatus ->
                                    article [] <|
                                        case ticketStatus of
                                            Parsing ->
                                                viewTicketPlaceholder model.placeholderCharacter

                                            Success ticket ->
                                                viewTicket ticket

                                            Failure { fileName, message } ->
                                                [ p [] <|
                                                    case fileName of
                                                        Just name ->
                                                            [ code [] [ text name ]
                                                            , text <| ": " ++ message
                                                            ]

                                                        Nothing ->
                                                            [ text message ]
                                                ]
                                )
                       )
                )


layout : List (Html Msg) -> Html Msg
layout =
    div [ class "layout" ]


layoutWithDragAndDrop : Model -> List (Html Msg) -> Html Msg
layoutWithDragAndDrop model =
    div
        [ hijackOn "dragenter" (Decode.succeed DragEnter)
        , hijackOn "dragover" (Decode.succeed DragEnter)
        , hijackOn "dragleave" (Decode.succeed DragLeave)
        , hijackOn "drop" filesDecoder
        , class "layout"
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
           ]


selectTicketInput : Html Msg
selectTicketInput =
    label [ class "file-input file-input--secondary" ]
        [ text "Select another bunch of tickets or drop it anywhere on the page"
        , input
            [ type_ "file"
            , accept "application/pdf"
            , on "change" filesDecoder
            , multiple True
            ]
            []
        ]


viewTicketPlaceholder : String -> List (Html Msg)
viewTicketPlaceholder placeholderCharacter =
    viewTicketDetails Nothing placeholderCharacter


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
