port module Main exposing (main)

import Browser
import File exposing (File)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode as Encode


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Model =
    ()


type Msg
    = GotFile Decode.Value


port parsePdf : Encode.Value -> Cmd msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( (), Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotFile file ->
            ( model, parsePdf file )


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
    div []
        [ input
            [ type_ "file"
            , accept "application/pdf"
            , on "change" (Decode.map GotFile fileDecoder)
            ]
            []
        ]
