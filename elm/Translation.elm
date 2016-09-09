module Translation exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Diff exposing (..)
import String


-- MODEL


type alias Model =
    { id : Int
    , source : String
    , target : String
    , state : State
    }


type alias State =
    { guess : String
    , checked : Bool
    }


initialState : State
initialState =
    { guess = ""
    , checked = False
    }



-- UPDATE


type Msg
    = UpdateGuess String
    | CheckGuess
    | Next


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateGuess guess ->
            let
                state =
                    model.state

                state' =
                    { state | guess = guess }
            in
                ({ model | state = state' })

        CheckGuess ->
            let
                state =
                    model.state

                state' =
                    { state | checked = True }
            in
                { model | state = state' }

        Next ->
            model



-- VIEW


diff : Model -> Html Msg
diff model =
    let
        parts =
            diffChars model.state.guess model.target

        viewPart int part =
            case part of
                NoChange str ->
                    span
                        [ style [ ( "color", "#666" ) ] ]
                        [ text str ]

                Changed str1 str2 ->
                    if int == 0 then
                        span
                            [ style [ ( "color", "#9a5ea7" ) ] ]
                            [ text str1 ]
                    else
                        span
                            [ style [ ( "color", "#9a5ea7" ) ] ]
                            [ text str2 ]

                Added str ->
                    if int == 0 then
                        span [] []
                    else
                        span
                            [ style [ ( "color", "#5ea766" ) ] ]
                            [ text str ]

                Removed str ->
                    if int == 0 then
                        span
                            [ style [ ( "color", "#c55353" ) ] ]
                            [ text str ]
                    else
                        span [] []
        part1 =
          if String.isEmpty model.state.guess
          then
            []
          else
            (List.map (viewPart 0) parts)
                ++ [ div
                     [ style
                       [ ("height", "10px")]
                     ]
                     []
                   ]
        part2 =
          (List.map (viewPart 1) parts)

    in
        div
            []
            (part1 ++ part2)


view : Model -> Html Msg
view model =
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
        if model.state.checked then
            Html.form
                [ onSubmit Next ]
                [ div
                    [ style
                        [ ( "font-size", "18px" )
                        , ( "color", "#333" )
                        , ( "margin-bottom", "10px" )
                        ]
                    ]
                    [ text model.source ]
                , div
                    [ style
                        [ ( "background-color", "#eee" )
                        , ( "padding", "20px" )
                        ]
                    ]
                    [ diff model ]
                , button
                    [ type' "submit"
                    , buttonStyle
                    ]
                    [ text "Next" ]
                ]
        else
            Html.form
                [ onSubmit CheckGuess ]
                [ div
                    [ style
                        [ ( "font-size", "18px" )
                        , ( "color", "#333" )
                        , ( "margin-bottom", "10px" )
                        ]
                    ]
                    [ text model.source ]
                , div
                    []
                    [ input
                        [ value model.state.guess
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
                    [ type' "submit"
                    , buttonStyle
                    ]
                    [ text "Check" ]
                ]
