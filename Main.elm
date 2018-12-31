port module Main exposing (main)

import Browser
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Debug
import Dict
import Dict.Extra exposing (..)
import Html exposing (Html, button, div, li, pre, text, ul)
import Html.Attributes exposing (classList)
import Html.Events exposing (onClick, onInput)
import Json.Decode as Decode exposing (Decoder, bool, decodeString, dict, float, int, keyValuePairs, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Maybe.Extra exposing (..)


test_results =
    """
  {
    "ReflectionsOnArrays": {
      "notice": "",
      "loc": 2290,
      "functions": {
        "arraysHaveLength": {
          "notice": "",
          "loc": 2479,
          "assertions": [
            "list should have 10 items"
            ],
            "fail": true
      }
    }
  },
  "ReflectionsOnNumbers": {
    "notice": "",
    "loc": 1181,
    "functions": {
      "numbersCanBeMultiplied": {
        "notice": "",
        "loc": 1757,
        "assertions": [],
        "fail": false
      },
      "numbersCanBeDivided": {
        "notice": "",
        "loc": 1227,
        "assertions": [
          "a should be greater than 1",
          "b should be greater than 1",
          "a should be twice as great as b"
          ],
          "fail": true
      }
    }
  },
  "ReflectionsOnTruth": {
    "notice": "You can use this contract for only the most basic simulation",
    "loc": 411,
    "functions": {
      "huh": {
        "notice": "To be quite honest.",
        "loc": 502,
        "assertions": [
          "value should now be false"
          ],
          "fail": true
      },
      "trueOrFalse": {
        "notice": "Consider that boolean values are either true/false.",
        "loc": 535,
        "assertions": [
          "value should now be false"
          ],
          "fail": true
      }
    }
  }
}
"""


functionDecoder : Decoder Function
functionDecoder =
    Decode.succeed Function
        |> required "assertions" (list string)
        |> required "notice" string
        |> required "loc" int
        |> required "fail" bool


contractDecoder : Decoder Contract
contractDecoder =
    Decode.succeed Contract
        |> required "functions" (keyValuePairs functionDecoder)
        |> required "notice" string
        |> required "loc" int


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias Function =
    { assertions : List String
    , notice : String
    , loc : Int
    , fail : Bool
    }


type alias Contract =
    { functions : List ( String, Function )
    , notice : String
    , loc : Int
    }


type alias Model =
    { testResults : TestResults
    , code : Code
    , quietForOneSecond : Debouncer Msg
    , error : List String
    }


port showResults : (Encode.Value -> msg) -> Sub msg


port showError : (Encode.Value -> msg) -> Sub msg


port showCode : (Encode.Value -> msg) -> Sub msg


port toJs : String -> Cmd msg


type alias TestResults =
    List ( String, Contract )


type alias ErrorResults =
    String


type alias Code =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    --( ( displayTestResults (decodeString (keyValuePairs contractDecoder) test_results), "" ), Cmd.none )
    ( { quietForOneSecond =
            Debouncer.manual
                |> settleWhenQuietFor (Just <| fromSeconds 1)
                |> toDebouncer
      , testResults = []
      , code = ""
      , error = []
      }
    , Cmd.none
    )



-- (([], ""), Cmd.none)
-- UPDATE


type Msg
    = DisplayTestResults TestResults
    | Run
    | Display String
    | Decode String
    | DisplayError (List String)
    | MsgQuietForOneSecond (Debouncer.Msg Msg)


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = MsgQuietForOneSecond
    , getDebouncer = .quietForOneSecond
    , setDebouncer = \debouncer model -> { model | quietForOneSecond = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgQuietForOneSecond subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        DisplayTestResults newTestResults ->
            ( { model | testResults = newTestResults }, Cmd.none )

        DisplayError error ->
            ( { model | error = error }, Cmd.none )

        Run ->
            ( { model | error = [] }, toJs model.code )

        Display newCode ->
            update (MsgQuietForOneSecond (provideInput Run)) { model | code = newCode }

        Decode value ->
            ( { model | testResults = decodeString value }, Cmd.none )



-- VIEW


displayAssertions : List String -> Html Msg
displayAssertions assertions =
    if List.length assertions > 0 then
        ul []
            (List.map
                (\a ->
                    li
                        []
                        [ text a
                        ]
                )
                assertions
            )

    else
        div [] []


displayFunction ( name, function ) =
    li
        [ classList
            [ ( "failed", function.fail )
            ]
        ]
        [ div [] [ text name ]
        , displayAssertions function.assertions
        ]


displayContract ( name, contract ) =
    li
        [ classList
            [ ( "failed", List.any (\( n, c ) -> c.fail) contract.functions )
            ]
        ]
        [ div [] [ text name ]
        , ul [] (List.map displayFunction (List.sortWith (\( name_a, function_a ) ( name_b, function_b ) -> compare function_a.loc function_b.loc) contract.functions))
        ]


displayTestResults contracts =
    List.sortWith (\( name_a, contract_a ) ( name_b, contract_b ) -> compare contract_a.loc contract_b.loc) contracts


displayError error =
    pre [] [ text error ]


view : Model -> Html Msg
view model =
    div []
        [ ul [] (List.map displayError model.error)
        , ul []
            (List.map displayContract model.testResults)
        , Html.textarea
            [ onInput Decode ]
            []
        , Html.textarea
            [ Html.Attributes.value model.code
            , onInput Display
            ]
            []
        ]


decodeString : String -> TestResults
decodeString x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeString (keyValuePairs contractDecoder) x
    in
    case result of
        Ok value ->
            displayTestResults value

        Err _ ->
            []


decodeError : Encode.Value -> Msg
decodeError x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (Decode.list (Decode.at [ "formattedMessage" ] Decode.string)) x
    in
    case result of
        Ok value ->
            DisplayError value

        Err value ->
            DisplayError [ Decode.errorToString value ]


decodeResults : Encode.Value -> Msg
decodeResults x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (keyValuePairs contractDecoder) x
    in
    case result of
        Ok value ->
            DisplayTestResults (displayTestResults value)

        Err value ->
            DisplayError [ Decode.errorToString value ]


decodeCode : Encode.Value -> Msg
decodeCode x =
    let
        result =
            Decode.decodeValue Decode.string x
    in
    case result of
        Ok value ->
            Display value

        Err value ->
            Display ("//" ++ Decode.errorToString value)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ showResults decodeResults
        , showError decodeError
        , showCode decodeCode
        ]
