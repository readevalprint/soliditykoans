port module Main exposing (main)

import Browser
import Debug
import Dict
import Dict.Extra exposing (..)
import Html exposing (Html, button, div, li, text, ul)
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


port toElm : (Encode.Value -> msg) -> Sub msg


port toJs : String -> Cmd msg


type alias Model =
    List ( String, Contract )


type alias Code =
    String


init : () -> ( ( Model, Code ), Cmd Msg )
init _ =
    --( ( displayTestResults (decodeString (keyValuePairs contractDecoder) test_results), "" ), Cmd.none )
    ( ( [], "" ), Cmd.none )



-- (([], ""), Cmd.none)
-- UPDATE


type Msg
    = Display Model
    | Run String
    | Decode String


update : Msg -> ( Model, Code ) -> ( ( Model, Code ), Cmd Msg )
update msg ( test, code ) =
    case msg of
        Display value ->
            ( ( value, code ), Cmd.none )

        Decode value ->
            ( ( decodeString value, code ), Cmd.none )

        Run str ->
            ( ( test, str ), toJs str )



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


view : ( Model, Code ) -> Html Msg
view ( tests, code ) =
    div []
        [ ul []
            (List.map displayContract tests)
        , Html.textarea
            [ onInput Decode ]
            []
        , Html.textarea
            [ Html.Attributes.value code, onInput Run ]
            []
        ]


decodeString : String -> Model
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


decodeValue : Encode.Value -> Msg
decodeValue x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (keyValuePairs contractDecoder) x
    in
    case result of
        Ok value ->
            Display (displayTestResults value)

        Err _ ->
            Display []


subscriptions : ( Model, Code ) -> Sub Msg
subscriptions ( tests, code ) =
    toElm decodeValue
