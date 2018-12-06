port module Main exposing (main)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, dict, float, int, list, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode exposing (Value)


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- TYPES


type alias Model =
    { message : String
    }


type Msg
    = UpdateStr String
    | SendToJs String



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { message = "Elm program is ready. Get started!" }, Cmd.none )



----- UPDATE


port toJs : String -> Cmd msg


port toElm : (Value -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateStr str ->
            ( { model | message = str }, Cmd.none )

        SendToJs str ->
            ( model, toJs str )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput UpdateStr, value model.message ] []
        , div [] [ text model.message ]
        , button
            [ onClick (SendToJs model.message) ]
            [ text "Send To JS" ]
        ]



-- SUBSCRIPTIONS


decodeValue : Value -> Msg
decodeValue x =
    let
        result =
            Decode.decodeValue Decode.string x
    in
    case result of
        Ok string ->
            UpdateStr string

        Err _ ->
            UpdateStr "Silly JavaScript, you can't kill me!"


subscriptions : Model -> Sub Msg
subscriptions model =
    toElm decodeValue
