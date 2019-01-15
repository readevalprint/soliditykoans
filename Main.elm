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


devDocFunctionDecoder : Decoder DevDocFunction
devDocFunctionDecoder =
    Decode.succeed DevDocFunction
        |> optional "details" string ""


devDocDecoder : Decoder DevDoc
devDocDecoder =
    Decode.succeed DevDoc
        |> optional "title" string "Untitled"
        |> optional "author" string "Anonomous"
        |> optional "details" string ""
        |> required "methods" (dict devDocFunctionDecoder)


main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }



-- MODEL


type alias DevDocFunction =
    { details : String
    }


type alias DevDoc =
    { title : String
    , author : String
    , details : String
    , methods : Dict String DevDocFunction
    }


type alias Model =
    { testResults : Maybe TestResult
    , quietForOneSecond : Debouncer Msg
    , error : List String
    , scroll : Scroll
    , languagesModel : Dict String LanguageModel
    , showLineCount : Bool
    , lineCountStart : Int
    , lineCount : Maybe Int
    , theme : String
    , highlight : HighlightModel
    , isTyping : Bool
    , isWaiting : Bool
    , docs : Maybe DevDoc
    }


type alias Scroll =
    { top : Int
    , left : Int
    }


port showResults : (Encode.Value -> msg) -> Sub msg


port showError : (Encode.Value -> msg) -> Sub msg


port showCode : (Encode.Value -> msg) -> Sub msg


port showDocs : (Encode.Value -> msg) -> Sub msg


port toJs : String -> Cmd msg


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
    , testResults = Nothing
    , error = []
    , scroll = Scroll 0 0
    , languagesModel = initLanguagesModel
    , showLineCount = True
    , lineCountStart = 1
    , lineCount = Just 1
    , theme = "Monokai"
    , highlight = initHighlightModel
    , isTyping = False
    , isWaiting = True
    , docs = Nothing
    }


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    }


type alias TestResult =
    { context : String
    , value : String
    , message : String
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
    = DisplayTestResults (Maybe TestResult)
    | OnScroll Scroll
    | Run
    | Display String
    | DisplayError (List String)
    | MsgQuietForOneSecond (Debouncer.Msg Msg)
    | Frame
    | SetDocs DevDoc


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
            ( { model | testResults = newTestResults, isWaiting = False }, Cmd.none )

        DisplayError error ->
            ( { model | error = error, isWaiting = False }, Cmd.none )

        Run ->
            ( { model | error = [], isTyping = False, isWaiting = True }, getLangModel "Javascript" model |> .code |> toJs )

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
                |> updateLangModel "Javascript" { model | isTyping = True }
                |> (\m -> update (MsgQuietForOneSecond (provideInput Run)) m)

        SetDocs docs ->
            ( { model | docs = Just docs }, Cmd.none )


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
                    , a [ class "link", href "https://twitter.com/soliditykoans" ] [ text "@soliditykoans" ]
                    ]
                , Html.section []
                    (case model.docs of
                        Just docs ->
                            [ Html.h1 [] [ text docs.title, Html.small [] [ text ("Author: " ++ docs.author) ] ]
                            , div []
                                [ Html.p [] [ text docs.details ]
                                , case model.isWaiting of
                                    True ->
                                        text ""

                                    False ->
                                        case model.testResults of
                                            Nothing ->
                                                div []
                                                    [ Html.h2 [] [ text "Success!" ]
                                                    , Html.p [] [ text "You have reached enlightenment. Or perhaps you have just deleted everything, which is a lesson in itself." ]
                                                    , Html.p [] [ text "This is my first elm-lang project. Follow it on twitter as I add more features." ]
                                                    ]

                                            Just tr ->
                                                case Dict.get (tr.value ++ "()") docs.methods of
                                                    Just m ->
                                                        Html.p [] [ text (m |> .details) ]

                                                    Nothing ->
                                                        text ""
                                ]
                            ]

                        Nothing ->
                            [ text "" ]
                    )
                , case model.isWaiting of
                    True ->
                        Html.div [ class "spinner" ] []

                    False ->
                        case model.testResults of
                            Nothing ->
                                text ""

                            Just tr ->
                                div
                                    [ class "card fluid"
                                    , case model.isTyping of
                                        True ->
                                            style "opacity" "0.5"

                                        False ->
                                            style "opacity" "1.0"
                                    ]
                                    [ div [ class "section" ]
                                        [ Html.strong [] [ text tr.message ]
                                        ]
                                    ]
                , div []
                    (model.error
                        |> List.map
                            (\error ->
                                div
                                    [ class "card  fluid" ]
                                    [ pre
                                        []
                                        [ text error
                                        ]
                                    ]
                            )
                    )
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
            DisplayError value

        Err value ->
            DisplayError [ Decode.errorToString value ]


testResultDecoder : Decoder TestResult
testResultDecoder =
    Decode.succeed TestResult
        |> required "context" string
        |> required "value" string
        |> required "message" string


decodeResults : Encode.Value -> Msg
decodeResults x =
    let
        result =
            -- Decode.decodeValue Decode.string x
            Decode.decodeValue (Decode.list testResultDecoder) x
    in
    case result of
        Ok value ->
            DisplayTestResults (value |> List.head)

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


decodeDocs : Encode.Value -> Msg
decodeDocs x =
    let
        result =
            Decode.decodeValue devDocDecoder x
    in
    case result of
        Ok value ->
            SetDocs value

        Err value ->
            DisplayError [ Decode.errorToString value ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ showResults decodeResults
        , showError decodeError
        , showCode decodeCode
        , showDocs decodeDocs
        , onAnimationFrame (\_ -> Frame)
        ]
