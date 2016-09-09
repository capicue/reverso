module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Reverso
import String
import Task exposing (Task)
import Translation


main : Program Never
main =
    App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { from : String
    , to : String
    , word : String
    , list : Array Translation.Model
    , index : Int
    , errorMessage : String
    }


initialModel : Model
initialModel =
    { from = ""
    , to = ""
    , word = ""
    , list = empty
    , index = 0
    , errorMessage = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = UpdateFrom String
    | UpdateTo String
    | UpdateWord String
    | SubmitWord
    | ReversoFail Http.Error
    | ReversoSucceed (List ( String, String ))
    | TranslationMsg Int Translation.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateFrom str ->
            ( { model | from = str }, Cmd.none )

        UpdateTo str ->
            ( { model | to = str }, Cmd.none )

        UpdateWord str ->
            ( { model | word = str }, Cmd.none )

        SubmitWord ->
            ( { model | index = 0, list = empty, errorMessage = "" }
            , Task.perform ReversoFail ReversoSucceed (Reverso.examples model.from model.to model.word)
            )

        ReversoFail err ->
            let
                msg =
                    case err of
                        Http.Timeout ->
                            "The request timed out. Please try again."

                        Http.NetworkError ->
                            "Network error. Check your connection."

                        Http.UnexpectedPayload str ->
                            "Server returned an unexpected response."

                        Http.BadResponse int str ->
                            "Server error " ++ (toString int) ++ " " ++ str
            in
                ( { model | errorMessage = msg }, Cmd.none )

        ReversoSucceed list ->
            let
                toTranslation i pair =
                    Translation.Model i (fst pair) (snd pair) Translation.initialState

                list' =
                    List.indexedMap toTranslation list
            in
                ( { model | list = (fromList list'), errorMessage = "" }
                , Cmd.none
                )

        TranslationMsg id msg ->
            case msg of
                Translation.Next ->
                    ( { model | index = model.index + 1 }, Cmd.none )

                _ ->
                    let
                        translation' =
                            Maybe.map (Translation.update msg) (get id model.list)

                        list' =
                            case translation' of
                                Just translation ->
                                    set id translation model.list

                                Nothing ->
                                    model.list
                    in
                        ( { model | list = list' }, Cmd.none )



-- VIEW


languageOption : String -> Html Msg
languageOption lang =
    option
        [ value lang ]
        [ text lang ]


view : Model -> Html Msg
view model =
    let
        buttonDisabled =
            (String.isEmpty model.from) || (String.isEmpty model.to) || (String.isEmpty model.word)

        translationView =
            case get model.index model.list of
                Just translation ->
                    App.map (TranslationMsg model.index) (Translation.view translation)

                Nothing ->
                    div [] []

        selectContainerStyle =
            style
                [ ( "width", "20%" )
                , ( "display", "inline-block" )
                , ( "padding-right", "5px" )
                , ( "box-sizing", "border-box" )
                ]

        selectStyle =
            style
                [ ( "border", "1px solid #aaa" )
                , ( "border-radius", "3px" )
                , ( "box-sizing", "border-box" )
                , ( "color", "#2b4564" )
                , ( "font-size", "16px" )
                , ( "height", "35px" )
                , ( "width", "100%" )
                ]
    in
        div
            [ style
                [ ( "margin", "20px auto 20px auto" )
                , ( "max-width", "800px" )
                ]
            ]
            [ div
                []
                [ text model.errorMessage ]
            , div
                [ style
                    [ ( "text-align", "center" )
                    , ( "font-size", "40px" )
                    , ( "margin-bottom", "80px" )
                    , ( "color", "#2b4564" )
                    , ( "font-weight", "bold" )
                    ]
                ]
                [ text "Translation Quiz" ]
            , Html.form
                [ onSubmit SubmitWord
                , style
                    [ ( "height", "36px" )
                    , ( "overflow", "hidden" )
                    ]
                ]
                [ div
                    [ selectContainerStyle ]
                    [ select
                        [ on "change" (JD.map UpdateFrom targetValue)
                        , selectStyle
                        ]
                        ([ option
                            [ value ""
                            , disabled True
                            , selected True
                            ]
                            [ text "From Language" ]
                         ]
                            ++ (List.map languageOption Reverso.languages)
                        )
                    ]
                , div
                    [ selectContainerStyle ]
                    [ select
                        [ on "change" (JD.map UpdateTo targetValue)
                        , selectStyle
                        ]
                        ([ option
                            [ value ""
                            , disabled True
                            , selected True
                            ]
                            [ text "To Language" ]
                         ]
                            ++ (List.map languageOption Reverso.languages)
                        )
                    ]
                , div
                    [ style
                        [ ( "width", "40%" )
                        , ( "display", "inline-block" )
                        , ( "padding", "0px" )
                        , ( "box-sizing", "border-box" )
                        ]
                    ]
                    [ input
                        [ onInput UpdateWord
                        , value model.word
                        , placeholder "Word"
                        , style
                            [ ( "border", "1px solid #aaa" )
                            , ( "border-radius", "5px" )
                            , ( "box-sizing", "border-box" )
                            , ( "color", "#2b4564" )
                            , ( "font-size", "16px" )
                            , ( "height", "35px" )
                            , ( "padding", "0px 6px" )
                            , ( "width", "100%" )
                            ]
                        ]
                        []
                    ]
                , div
                    [ style
                        [ ( "width", "20%" )
                        , ( "display", "inline-block" )
                        , ( "box-sizing", "border-box" )
                        , ( "padding-left", "5px" )
                        ]
                    ]
                    [ button
                        [ type' "submit"
                        , disabled buttonDisabled
                        , style
                            [ ( "background-color", "#2b4564" )
                            , ( "border", "1px solid #000" )
                            , ( "border-radius", "3px" )
                            , ( "box-sizing", "border-box" )
                            , ( "color", "white" )
                            , ( "font-size", "16px" )
                            , ( "height", "35px" )
                            , ( "width", "100%" )
                            ]
                        ]
                        [ text "Search" ]
                    ]
                ]
            , div
                [ style
                    [ ( "margin", "50px 0 0 0" )
                    ]
                ]
                [ translationView ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
