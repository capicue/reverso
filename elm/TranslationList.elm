module TranslationList exposing (..)

import Diff exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateGuess guess ->
            updateCurrent (updateGuess guess) model

        CheckGuess ->
            updateCurrent setChecked model

        Next ->
            case model.unseen of
                x :: xs ->
                    { model | seen = model.seen ++ [ x ], unseen = xs }

                _ ->
                    model

        EnterPressed ->
            case current model of
                Just translation ->
                    if translation.state.checked then
                        update Next model
                    else
                        update CheckGuess model

                Nothing ->
                    model


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

        grey =
            "#666"

        purple =
            "#9a5ea7"

        red =
            "#c55353"

        green =
            "#5ea766"

        viewSource part =
            case part of
                NoChange str ->
                    span
                        [ style [ ( "color", grey ) ] ]
                        [ text str ]

                Changed str1 str2 ->
                    span
                        [ style [ ( "color", purple ) ] ]
                        [ text str1 ]

                Added str ->
                    span [] []

                Removed str ->
                    span
                        [ style [ ( "color", red ) ] ]
                        [ text str ]

        viewTarget part =
            case part of
                NoChange str ->
                    span
                        [ style [ ( "color", grey ) ] ]
                        [ text str ]

                Changed str1 str2 ->
                    span
                        [ style [ ( "color", purple ) ] ]
                        [ text str2 ]

                Added str ->
                    span
                        [ style [ ( "color", green ) ] ]
                        [ text str ]

                Removed str ->
                    span [] []

        part1 =
            if String.isEmpty str1 then
                []
            else
                (List.map viewSource parts)
                    ++ [ div
                            [ style
                                [ ( "height", "10px" ) ]
                            ]
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
        buttonStyle =
            style
                [ ( "margin-top", "20px" )
                , ( "font-size", "14px" )
                , ( "height", "auto" )
                , ( "background-color", "#2b4564" )
                , ( "color", "white" )
                , ( "border-radius", "3px" )
                , ( "border", "1px solid #000" )
                , ( "padding", "6px" )
                ]
    in
        if translation.state.checked then
            div
                []
                [ div
                    [ style
                        [ ( "font-size", "18px" )
                        , ( "color", "#333" )
                        , ( "margin-bottom", "10px" )
                        ]
                    ]
                    [ text translation.source ]
                , div
                    [ style
                        [ ( "background-color", "#eee" )
                        , ( "padding", "20px" )
                        ]
                    ]
                    [ diff translation.state.guess translation.target ]
                , button
                    [ buttonStyle
                    , onClick Next
                    ]
                    [ text "Next" ]
                ]
        else
            div
                []
                [ div
                    [ style
                        [ ( "font-size", "18px" )
                        , ( "color", "#333" )
                        , ( "margin-bottom", "10px" )
                        ]
                    ]
                    [ text translation.source ]
                , div
                    []
                    [ input
                        [ value translation.state.guess
                        , onInput UpdateGuess
                        , placeholder "Guess"
                        , style
                            [ ( "width", "100%" )
                            , ( "font-size", "18px" )
                            , ( "padding", "6px" )
                            , ( "border-radius", "3px" )
                            , ( "border", "1px solid #333" )
                            , ( "box-sizing", "border-box" )
                            ]
                        ]
                        []
                    ]
                , button
                    [ buttonStyle
                    , onClick CheckGuess
                    ]
                    [ text "Check" ]
                ]
