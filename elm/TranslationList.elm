module TranslationList exposing (..)

import Diff exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Styles


-- MODEL


type alias Translation =
    { source : String
    , target : String
    , state : TranslationState
    }


type alias TranslationState =
    { guess : String
    , checked : Bool
    }


initialTranslationState : TranslationState
initialTranslationState =
    { guess = ""
    , checked = False
    }


type alias Model =
    { seen : List Translation
    , unseen : List Translation
    }


current : Model -> Maybe Translation
current model =
    List.head model.unseen


isEmpty : Model -> Bool
isEmpty model =
    List.isEmpty model.seen && List.isEmpty model.unseen



-- UPDATE


type Msg
    = UpdateGuess String
    | CheckGuess
    | Next
    | EnterPressed


type ParentMsg
    = FocusGuess
    | NoOp


update : Msg -> Model -> ( Model, ParentMsg )
update msg model =
    case msg of
        UpdateGuess guess ->
            ( updateCurrent (updateGuess guess) model, NoOp )

        CheckGuess ->
            ( updateCurrent setChecked model, NoOp )

        Next ->
            case model.unseen of
                x :: xs ->
                    ( { model | seen = model.seen ++ [ x ], unseen = xs }, FocusGuess )

                _ ->
                    ( model, NoOp )

        EnterPressed ->
            case current model of
                Just translation ->
                    if translation.state.checked then
                        update Next model
                    else
                        update CheckGuess model

                Nothing ->
                    ( model, NoOp )


updateCurrent : (Translation -> Translation) -> Model -> Model
updateCurrent function model =
    case model.unseen of
        x :: xs ->
            { model | unseen = (function x) :: xs }

        _ ->
            model


updateGuess : String -> Translation -> Translation
updateGuess guess translation =
    let
        state =
            translation.state

        state' =
            { state | guess = guess }
    in
        { translation | state = state' }


setChecked : Translation -> Translation
setChecked translation =
    let
        state =
            translation.state

        state' =
            { state | checked = True }
    in
        { translation | state = state' }



-- VIEW


styles =
    { container =
        [ ( "font-size", "18px" )
        , ( "color", "#333" )
        , ( "margin-bottom", "10px" )
        ]
    , diffContainer =
        [ ( "background-color", "#eee" )
        , ( "padding", "20px" )
        ]
    , partsSeparator =
        [ ( "height", "10px" ) ]
    , buttonCheck =
        [ ( "margin-top", "20px" )
        , ( "font-size", "14px" )
        , ( "height", "auto" )
        , ( "background-color", "#2b4564" )
        , ( "color", "white" )
        , ( "border-radius", "3px" )
        , ( "border", "1px solid black" )
        , ( "padding", "6px" )
        ]
    , guess =
        [ ( "width", "100%" )
        , ( "font-size", "18px" )
        , ( "padding", "6px" )
        , ( "border-radius", "3px" )
        , ( "border", "1px solid #333" )
        , ( "box-sizing", "border-box" )
        ]
    , grey =
        [ ( "color", (.grey) Styles.colors ) ]
    , red =
        [ ( "color", (.red) Styles.colors ) ]
    , green =
        [ ( "color", (.green) Styles.colors ) ]
    , purple =
        [ ( "color", (.purple) Styles.colors ) ]
    }


view : Model -> Html Msg
view model =
    case current model of
        Just translation ->
            viewTranslation translation

        Nothing ->
            let
                content =
                    if isEmpty model then
                        "No results. Try something else!"
                    else
                        "That's it. Try a new word or phrase!"
            in
                div
                    []
                    [ text content ]


diff : String -> String -> Html Msg
diff str1 str2 =
    let
        parts =
            diffChars str1 str2

        viewSource part =
            case part of
                NoChange str ->
                    span
                        [ style styles.grey ]
                        [ text str ]

                Changed str1 str2 ->
                    span
                        [ style styles.purple ]
                        [ text str1 ]

                Added str ->
                    span [] []

                Removed str ->
                    span
                        [ style styles.red ]
                        [ text str ]

        viewTarget part =
            case part of
                NoChange str ->
                    span
                        [ style styles.grey ]
                        [ text str ]

                Changed str1 str2 ->
                    span
                        [ style styles.purple ]
                        [ text str2 ]

                Added str ->
                    span
                        [ style styles.green ]
                        [ text str ]

                Removed str ->
                    span [] []

        part1 =
            if String.isEmpty str1 then
                []
            else
                (List.map viewSource parts)
                    ++ [ div
                            [ style styles.partsSeparator ]
                            []
                       ]

        part2 =
            (List.map viewTarget parts)
    in
        div
            []
            (part1 ++ part2)


viewTranslation : Translation -> Html Msg
viewTranslation translation =
    let
        checked =
            translation.state.checked

        content =
            if checked then
                div
                    [ style styles.diffContainer ]
                    [ diff translation.state.guess translation.target ]
            else
                div
                    []
                    [ input
                        [ id "guess"
                        , value translation.state.guess
                        , onInput UpdateGuess
                        , placeholder "Guess"
                        , style styles.guess
                        ]
                        []
                    ]

        checkOrNextButton =
            if checked then
                button
                    [ style styles.buttonCheck
                    , onClick Next
                    ]
                    [ text "Next" ]
            else
                button
                    [ style styles.buttonCheck
                    , onClick CheckGuess
                    ]
                    [ text "Check" ]
    in
        div
            []
            [ div
                [ style styles.container
                ]
                [ text translation.source
                , content
                , checkOrNextButton
                ]
            ]
