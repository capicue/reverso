import Html exposing (..)
import Html.App as App

main : Program Never
main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model = {}

init : (Model, Cmd Msg)
init = ({}, Cmd.none)

-- UPDATE

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      (model, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
  text "Hello"

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
