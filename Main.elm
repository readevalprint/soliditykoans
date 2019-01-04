port module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Debug
import Dict exposing (Dict)
import Dict.Extra exposing (..)
import Html exposing (Html, button, code, div, li, pre, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, href, id, placeholder, selected, spellcheck, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy
import Json.Decode as Decode exposing (Decoder, at, bool, decodeString, dict, float, int, keyValuePairs, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Maybe.Extra exposing (..)
import SyntaxHighlight as SH exposing (monokai, toBlockHtml, useTheme)


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
    , quietForOneSecond : Debouncer Msg
    , error : List String
    , scroll : Scroll
    , currentLanguage : String
    , languagesModel : Dict String LanguageModel
    , showLineCount : Bool
    , lineCountStart : Int
    , lineCount : Maybe Int
    , theme : String
    , customTheme : String
    }


type alias Scroll =
    { top : Int
    , left : Int
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


initModel : Model
initModel =
    { quietForOneSecond =
        Debouncer.manual
            |> settleWhenQuietFor (Just <| fromSeconds 1)
            |> toDebouncer
    , testResults = []
    , error = []
    , scroll = Scroll 0 0
    , currentLanguage = "Javascript"
    , languagesModel = initLanguagesModel
    , showLineCount = True
    , lineCountStart = 1
    , lineCount = Just 1
    , theme = "Monokai"
    , customTheme = rawMonokai
    }


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    }


initLanguageModel : String -> LanguageModel
initLanguageModel codeStr =
    { code = codeStr
    , scroll = Scroll 0 0
    }


initLanguagesModel : Dict String LanguageModel
initLanguagesModel =
    Dict.fromList
        [ ( "Javascript", initLanguageModel javascriptExample )
        ]


javascriptExample : String
javascriptExample =
    """
var a = 1;
    """


rawMonokai : String
rawMonokai =
    ".elmsh {color: #f8f8f2;background: #23241f;}.elmsh-hl {background: #343434;}.elmsh-add {background: #003800;}.elmsh-del {background: #380000;}.elmsh-comm {color: #75715e;}.elmsh1 {color: #ae81ff;}.elmsh2 {color: #e6db74;}.elmsh3 {color: #f92672;}.elmsh4 {color: #66d9ef;}.elmsh5 {color: #a6e22e;}.elmsh6 {color: #ae81ff;}.elmsh7 {color: #fd971f;}.elmsh-elm-ts, .elmsh-js-dk, .elmsh-css-p {font-style: italic;color: #66d9ef;}.elmsh-js-ce {font-style: italic;color: #a6e22e;}.elmsh-css-ar-i {font-weight: bold;color: #f92672;}"


init : () -> ( Model, Cmd Msg )
init _ =
    --( ( displayTestResults (decodeString (keyValuePairs contractDecoder) test_results), "" ), Cmd.none )
    ( initModel
    , Cmd.none
    )



-- (([], ""), Cmd.none)
-- UPDATE


type Msg
    = DisplayTestResults TestResults
    | SetText String String
    | OnScroll Scroll
    | Run
    | Display String
    | Decode String
    | DisplayError (List String)
    | MsgQuietForOneSecond (Debouncer.Msg Msg)
    | Frame


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
            ( { model | error = [] }, getLangModel "Javascript" model |> .code |> toJs )

        Display newCode ->
            update (MsgQuietForOneSecond (provideInput Run)) model

        Decode value ->
            ( { model | testResults = decodeString value }, Cmd.none )

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Frame ->
            getLangModel model.currentLanguage model
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel model.currentLanguage model
                |> (\a -> ( a, Cmd.none ))

        SetText lang codeStr ->
            getLangModel lang model
                |> (\m -> { m | code = codeStr })
                |> updateLangModel lang model
                |> (\m -> update (MsgQuietForOneSecond (provideInput Run)) m)


updateLangModel : String -> Model -> LanguageModel -> Model
updateLangModel lang model langModel =
    Dict.insert lang langModel model.languagesModel
        |> (\n -> { model | languagesModel = n })



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
            [ Html.Attributes.value ""
            , onInput Display
            ]
            []
        , div [] [ useTheme monokai ]
        , div
            []
            [ viewLanguage "Javascript" toHtml model
            ]
        ]


getLangModel : String -> Model -> LanguageModel
getLangModel lang model =
    Dict.get lang model.languagesModel
        |> Maybe.withDefault (initLanguageModel javascriptExample)


toHtml : Maybe Int -> String -> Html Msg
toHtml maybeStart str =
    SH.javascript str
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.withDefault
            (pre [] [ code [] [ text str ] ])


viewLanguage : String -> (Maybe Int -> String -> Html Msg) -> Model -> Html Msg
viewLanguage thisLang parser ({ currentLanguage, lineCount } as model) =
    if thisLang /= currentLanguage then
        div [] []

    else
        let
            langModel =
                getLangModel thisLang model
        in
        div
            [ classList
                [ ( "container", True )
                , ( "elmsh", True )
                ]
            ]
            [ div
                [ class "view-container"
                , style "transform"
                    ("translate("
                        ++ String.fromInt -langModel.scroll.left
                        ++ "px, "
                        ++ String.fromInt -langModel.scroll.top
                        ++ "px)"
                    )
                , style "will-change" "transform"
                ]
                [ parser
                    lineCount
                    langModel.code
                ]
            , viewTextarea thisLang langModel.code model
            ]


viewTextarea : String -> String -> Model -> Html Msg
viewTextarea thisLang codeStr { showLineCount } =
    textarea
        [ value codeStr
        , classList
            [ ( "textarea", True )
            , ( "textarea-lc", showLineCount )
            ]
        , onInput (SetText thisLang)
        , spellcheck False
        , Html.Events.on "scroll"
            (Decode.map2 Scroll
                (Decode.at [ "target", "scrollTop" ] Decode.int)
                (Decode.at [ "target", "scrollLeft" ] Decode.int)
                |> Decode.map OnScroll
            )
        ]
        []


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
        , onAnimationFrame (\_ -> Frame)
        ]
