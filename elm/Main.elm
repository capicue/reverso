module Main exposing (..)

import Array exposing (..)
import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD
import Keyboard
import RemoteData exposing (..)
import Reverso
import Spinner
import String
import Task exposing (Task)
import TranslationList


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
    , list : WebData (TranslationList.Model)
    , spinner : Spinner.Model
    }


initialModel : Model
initialModel =
    { from = ""
    , to = ""
    , word = ""
    , list = NotAsked
    , spinner = Spinner.init
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = UpdateFrom String
    | UpdateTo String
    | UpdateWord String
    | SpinnerMsg Spinner.Msg
    | SubmitWord
    | ReversoResponse (WebData TranslationList.Model)
    | TranslationListMsg TranslationList.Msg
    | KeyPress Keyboard.KeyCode


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
            ( { model | list = Loading }
            , Cmd.map ReversoResponse (RemoteData.asCmd (Reverso.examples model.from model.to model.word))
            )

        ReversoResponse res ->
            ( { model | list = res }, Cmd.none )

        TranslationListMsg msg ->
            case model.list of
                Success list ->
                    let
                        list' =
                            TranslationList.update msg list
                    in
                        ( { model | list = Success list' }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        SpinnerMsg msg ->
            let
                spinnerModel =
                    Spinner.update msg model.spinner
            in
                ( { model | spinner = spinnerModel }, Cmd.none )

        KeyPress code ->
            case code of
                13 ->
                    case model.list of
                        Success list ->
                            let
                                list' =
                                    TranslationList.update TranslationList.EnterPressed list
                            in
                                ( { model | list = Success list' }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )



-- VIEW


languageOption : String -> Html Msg
languageOption lang =
    option
        [ value lang ]
        [ text lang ]


styles =
    { body =
        [ ( "margin", "0px auto" )
        , ( "padding", "0" )
        , ( "height", "100%" )
        , ( "max-width", "800px" )
        , ( "color", "#2b4564" )
        ]
    , container =
        [ ( "height", "100%" )
        , ( "position", "relative" )
        ]
    , content =
        [ ( "padding", "60px 10px" )
        ]
    , title =
        [ ( "text-align", "center" )
        , ( "font-size", "40px" )
        , ( "margin-bottom", "80px" )
        , ( "font-weight", "bold" )
        ]
    , formWord =
        [ ( "height", "36px" )
        , ( "overflow", "hidden" )
        ]
    , selectContainer =
        [ ( "width", "20%" )
        , ( "display", "inline-block" )
        , ( "padding-right", "5px" )
        , ( "box-sizing", "border-box" )
        ]
    , select =
        [ ( "border", "1px solid #aaa" )
        , ( "border-radius", "3px" )
        , ( "box-sizing", "border-box" )
        , ( "color", "#2b4564" )
        , ( "font-size", "16px" )
        , ( "height", "35px" )
        , ( "width", "100%" )
        ]
    , wordContainer =
        [ ( "width", "40%" )
        , ( "display", "inline-block" )
        , ( "padding", "0px" )
        , ( "box-sizing", "border-box" )
        ]
    , word =
        [ ( "border", "1px solid #aaa" )
        , ( "border-radius", "5px" )
        , ( "box-sizing", "border-box" )
        , ( "color", "#2b4564" )
        , ( "font-size", "16px" )
        , ( "height", "35px" )
        , ( "padding", "0px 6px" )
        , ( "width", "100%" )
        ]
    , searchContainer =
        [ ( "width", "20%" )
        , ( "display", "inline-block" )
        , ( "box-sizing", "border-box" )
        , ( "padding-left", "5px" )
        ]
    , buttonSearch =
        [ ( "background-color", "#2b4564" )
        , ( "border", "1px solid black" )
        , ( "border-radius", "3px" )
        , ( "box-sizing", "border-box" )
        , ( "color", "white" )
        , ( "font-size", "16px" )
        , ( "height", "35px" )
        , ( "width", "100%" )
        ]
    , disabled =
        [ ( "opacity", "0.5" )
        ]
    , translationContainer =
        [ ( "margin", "50px 0 0 0" ) ]
    , footer =
        [ ( "position", "absolute" )
        , ( "bottom", "0" )
        , ( "width", "100%" )
        , ( "height", "60px" )
        , ( "text-align", "center" )
        ]
    , footerLink =
        [ ( "text-decoration", "none" )
        , ( "color", "#006ea6" )
        ]
    }


view : Model -> Html Msg
view model =
    let
        buttonDisabled =
            (String.isEmpty model.from) || (String.isEmpty model.to) || (String.isEmpty model.word) || model.from == model.to

        buttonSearchStyle =
            if buttonDisabled then
                style (styles.buttonSearch ++ styles.disabled)
            else
                style styles.buttonSearch

        spinnerConfig =
            let
                config =
                    Spinner.defaultConfig
            in
                { config | translateX = 0, translateY = 0 }

        translationView =
            case model.list of
                NotAsked ->
                    div []
                        []

                Loading ->
                    div
                        []
                        [ Spinner.view Spinner.defaultConfig model.spinner ]

                Failure err ->
                    case err of
                        Http.Timeout ->
                            div [] [ text "The request timed out. Please try again." ]

                        Http.NetworkError ->
                            div [] [ text "Network error. Check your connection." ]

                        Http.UnexpectedPayload str ->
                            div [] [ text "Server returned an unexpected response." ]

                        Http.BadResponse int str ->
                            div [] [ text ("Server error " ++ (toString int) ++ " " ++ str) ]

                Success list ->
                    App.map TranslationListMsg (TranslationList.view list)
    in
        div
            [ style styles.body ]
            [ div
                [ style styles.container ]
                [ div
                    [ style styles.content ]
                    [ div
                        [ style styles.title ]
                        [ text "Translation Quiz" ]
                    , Html.form
                        [ onSubmit SubmitWord
                        , style styles.formWord
                        ]
                        [ div
                            [ style styles.selectContainer ]
                            [ select
                                [ on "change" (JD.map UpdateFrom targetValue)
                                , style styles.select
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
                            [ style styles.selectContainer ]
                            [ select
                                [ on "change" (JD.map UpdateTo targetValue)
                                , style styles.select
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
                            [ style styles.wordContainer ]
                            [ input
                                [ onInput UpdateWord
                                , value model.word
                                , placeholder "Word or Phrase"
                                , style styles.word
                                ]
                                []
                            ]
                        , div
                            [ style styles.searchContainer ]
                            [ button
                                [ type' "submit"
                                , disabled buttonDisabled
                                , buttonSearchStyle
                                ]
                                [ text "Search" ]
                            ]
                        ]
                    , div
                        [ style styles.translationContainer ]
                        [ translationView ]
                    ]
                , div
                    [ style styles.footer ]
                    [ div
                        []
                        [ text "Built with "
                        , a
                            [ href "https://runkit.com/capicue/reverso"
                            , style styles.footerLink
                            ]
                            [ text "RunKit" ]
                        , text " and "
                        , a
                            [ href "http://elm-lang.org/"
                            , style styles.footerLink
                            ]
                            [ text "Elm" ]
                        , text ". Data from "
                        , a
                            [ href "http://context.reverso.net/translation/"
                            , style styles.footerLink
                            ]
                            [ text "Reverso Context" ]
                        , text "."
                        ]
                    ]
                ]
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SpinnerMsg Spinner.subscription
        , Keyboard.presses KeyPress
        ]
