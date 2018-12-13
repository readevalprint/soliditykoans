port module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, dict, field, float, int, list, map2, string)
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


type alias Message =
    { title : String
    , body : String
    }


type alias Model =
    { next : Message
    , messages : List Message
    }


type Msg
    = AddMessage
    | SendToJs
    | ClearNext
    | UpdateNext Message
    | SetField FormField String
    | Noop



-- MODEL


init : () -> ( Model, Cmd Msg )
init _ =
    ( { next = Message "first" "message", messages = [ Message "a title" "Elm program is ready. Get started!" ] }, Cmd.none )



----- UPDATE


type FormField
    = Title
    | Body


setField : Model -> FormField -> String -> Model
setField model field value =
    case field of
        Title ->
            { model | next = Message value model.next.body }

        Body ->
            { model | next = Message model.next.title value }


port toJs : Model -> Cmd msg


port toElm : (Value -> msg) -> Sub msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendToJs ->
            ( model, toJs model )

        UpdateNext next ->
            ( { model | next = next }, Cmd.none )

        AddMessage ->
            ( { model | next = Message "" "", messages = model.messages ++ [ model.next ] }, Cmd.none )

        SetField field value ->
            ( setField model field value, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- VIEW


onEnter : msg -> Attribute msg
onEnter msg =
    keyCode
        |> Decode.andThen
            (\key ->
                if key == 13 then
                    Decode.succeed msg

                else
                    Decode.fail "Not enter"
            )
        |> on "keyup"


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label []
                [ text "title"
                , input
                    [ type_ "text"
                    , name "title"
                    , value model.next.title
                    , onInput <| SetField Title
                    ]
                    []
                ]
            , br [] []
            , label []
                [ text "body"
                , input
                    [ type_ "text"
                    , name "body"
                    , value model.next.body
                    , onInput <| SetField Body
                    ]
                    []
                ]
            , button [ onClick SendToJs ] [ text "Send to JS" ]
            , button [ onClick AddMessage ] [ text "Add Message" ]
            ]
        ]



-- SUBSCRIPTIONS


messageDecoder : Decoder Message
messageDecoder =
    map2 Message
        (field "title" string)
        (field "body" string)


decodeValue : Value -> Msg
decodeValue x =
    let
        result =
            Decode.decodeValue messageDecoder x
    in
    case result of
        Ok next ->
            UpdateNext next

        Err e ->
            Noop


subscriptions : Model -> Sub Msg
subscriptions model =
    toElm decodeValue
