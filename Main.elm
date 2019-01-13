port module Main exposing (main)

import Browser
import Browser.Events exposing (onAnimationFrame)
import Debouncer.Messages as Debouncer exposing (Debouncer, fromSeconds, provideInput, settleWhenQuietFor, toDebouncer)
import Dict exposing (Dict)
import Dict.Extra exposing (..)
import Html exposing (Html, a, button, code, div, header, img, li, node, pre, text, textarea, ul)
import Html.Attributes exposing (checked, class, classList, href, id, placeholder, selected, spellcheck, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Html.Lazy
import Json.Decode as Decode exposing (Decoder, at, bool, decodeString, dict, float, int, keyValuePairs, list, nullable, string)
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Maybe.Extra exposing (..)
import SyntaxHighlight as SH exposing (monokai, toBlockHtml, useTheme)


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
    { testResults : String
    , quietForOneSecond : Debouncer Msg
    , error : String
    , scroll : Scroll
    , languagesModel : Dict String LanguageModel
    , showLineCount : Bool
    , lineCountStart : Int
    , lineCount : Maybe Int
    , theme : String
    , highlight : HighlightModel
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


type alias HighlightModel =
    { mode : Maybe SH.Highlight
    , start : Int
    , end : Int
    }


initHighlightModel : HighlightModel
initHighlightModel =
    { mode = Just SH.Highlight
    , start = 0
    , end = 0
    }


lineNumberFromError : List String -> String -> Int
lineNumberFromError path code =
    List.map (\s -> String.indexes s code) path
        |> List.foldl
            (\state i ->
                List.filter (\i2 -> i2 > i) state
                    |> List.head
                    |> Maybe.withDefault 0
            )
            -- default value
            0
        -- Find the actual line number (number of newline before the string index)
        |> (\index ->
                String.indexes "\n" code
                    |> List.filter (\n -> n < index)
                    |> List.length
           )


initModel : Model
initModel =
    { quietForOneSecond =
        Debouncer.manual
            |> settleWhenQuietFor (Just <| fromSeconds 1)
            |> toDebouncer
    , testResults = ""
    , error = ""
    , scroll = Scroll 0 0
    , languagesModel = initLanguagesModel
    , showLineCount = True
    , lineCountStart = 1
    , lineCount = Just 1
    , theme = "Monokai"
    , highlight = initHighlightModel
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
        [ ( "Javascript", initLanguageModel "" )
        ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( initModel
    , Cmd.none
    )



-- (([], ""), Cmd.none)
-- UPDATE


type Msg
    = DisplayTestResults String
    | OnScroll Scroll
    | Run
    | Display String
    | Decode String
    | DisplayError String
    | MsgQuietForOneSecond (Debouncer.Msg Msg)
    | Frame


updateDebouncer : Debouncer.UpdateConfig Msg Model
updateDebouncer =
    { mapMsg = MsgQuietForOneSecond
    , getDebouncer = .quietForOneSecond
    , setDebouncer = \debouncer model -> { model | quietForOneSecond = debouncer }
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ highlight } as model) =
    case msg of
        MsgQuietForOneSecond subMsg ->
            Debouncer.update update updateDebouncer subMsg model

        DisplayTestResults newTestResults ->
            ( { model | testResults = newTestResults }, Cmd.none )

        DisplayError error ->
            ( { model | error = error }, Cmd.none )

        Run ->
            ( { model | error = "" }, getLangModel "Javascript" model |> .code |> toJs )

        Decode value ->
            ( { model | testResults = value }, Cmd.none )

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Frame ->
            getLangModel "Javascript" model
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel "Javascript" model
                |> (\a -> ( a, Cmd.none ))

        Display codeStr ->
            getLangModel "Javascript" model
                |> (\m -> { m | code = codeStr })
                |> updateLangModel "Javascript" model
                |> (\m -> update (MsgQuietForOneSecond (provideInput Run)) m)


updateLangModel : String -> Model -> LanguageModel -> Model
updateLangModel lang model langModel =
    Dict.insert lang langModel model.languagesModel
        |> (\n -> { model | languagesModel = n })


displayError error =
    pre [] [ text error ]


view : Model -> Html Msg
view model =
    div
        [ classList
            [ ( "container", True )
            ]
        ]
        [ div
            [ classList
                [ ( "row", True )
                , ( "cols-sm-6", True )
                ]
            ]
            [ viewLanguage "Javascript" toHtml model
            , Html.section []
                [ header []
                    [ a [ class "logo" ] [ text "Solidity Koans" ]
                    , a [ class "button", href "https://twitter.com/soliditykoans" ] [ text "@soliditykoans" ]
                    ]
                , div [ class "card fluid" ]
                    [ div [ class "section" ]
                        [ Html.h1 [] [ text model.testResults ]
                        ]
                    ]
                , div
                    [ class "card  fluid" ]
                    [ pre
                        []
                        [ text model.error
                        ]
                    ]
                ]
            ]
        ]


getLangModel : String -> Model -> LanguageModel
getLangModel lang model =
    Dict.get lang model.languagesModel
        |> Maybe.withDefault (initLanguageModel "")


toHtml : Maybe Int -> String -> HighlightModel -> Html Msg
toHtml maybeStart str hlModel =
    SH.javascript str
        |> Result.map (SH.highlightLines hlModel.mode hlModel.start hlModel.end)
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.withDefault
            (div [] [ text str ])


viewLanguage : String -> (Maybe Int -> String -> HighlightModel -> Html Msg) -> Model -> Html Msg
viewLanguage thisLang parser ({ lineCount, highlight } as model) =
    let
        langModel =
            getLangModel thisLang model
    in
    div
        [ classList
            [ ( "elmsh", True )
            , ( "elmsh-container", True )
            ]
        ]
        [ div
            [ class "view-container "
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
                highlight
            ]
        , viewTextarea thisLang langModel.code model
        ]


viewTextarea : String -> String -> Model -> Html Msg
viewTextarea thisLang codeStr { showLineCount } =
    div []
        [ node "style" [] [ text (".textarea, .view-container {height: " ++ ((toFloat (codeStr |> String.indexes "\n" |> List.length) * 1.6) |> String.fromFloat) ++ "rem !important;}") ]
        , textarea
            [ value codeStr
            , classList
                [ ( "textarea", True )
                , ( "textarea-lc", showLineCount )
                ]
            , onInput Display
            , spellcheck False
            , Html.Events.on "scroll"
                (Decode.map2 Scroll
                    (Decode.at [ "target", "scrollTop" ] Decode.int)
                    (Decode.at [ "target", "scrollLeft" ] Decode.int)
                    |> Decode.map OnScroll
                )
            ]
            []
        ]


decodeError : Encode.Value -> Msg
decodeError x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (Decode.list (Decode.at [ "formattedMessage" ] Decode.string)) x
    in
    case result of
        Ok value ->
            DisplayError (value |> List.head |> Maybe.withDefault "")

        Err value ->
            DisplayError (Decode.errorToString value)


decodeResults : Encode.Value -> Msg
decodeResults x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (Decode.list (Decode.at [ "message" ] Decode.string)) x
    in
    case result of
        Ok value ->
            DisplayTestResults (value |> List.head |> Maybe.withDefault "")

        Err value ->
            DisplayTestResults (Decode.errorToString value)


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
