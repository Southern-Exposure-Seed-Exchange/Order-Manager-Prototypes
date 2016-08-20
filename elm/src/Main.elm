module Main exposing (..)

import Html exposing (Html, div, text)
import Html.App


type alias Model =
    String


init : ( Model, Cmd Msg )
init =
    ( "Hello World", Cmd.none )


type Msg
    = NoOp


view : Model -> Html Msg
view model =
    div []
        [ text model ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
