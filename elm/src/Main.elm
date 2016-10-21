module Main exposing (..)

{-| This is the entry point of the Elm application, it combines an initial
module with the view and update functions to create a `Program`.

@docs init, urlUpdate, main

-}

import Navigation
import Commands exposing (fetchForRoute, setPageTitle)
import Messages exposing (Msg)
import Models exposing (Model, initialModel, UIState, initialUIState)
import Routing exposing (Route)
import Subscriptions exposing (subscriptions)
import Update exposing (update, updateUI)
import View exposing (view)


{-| The initial model and commands for the application are generated from the
Route. The Page Title is set & any relevant data is fetched from the backend.
-}
init : Result String Route -> ( Model, Cmd Msg )
init result =
    let
        currentRoute =
            Routing.routeFromResult result
    in
        ( initialModel currentRoute
        , Cmd.batch
            [ setPageTitle currentRoute
            , fetchForRoute currentRoute
            ]
        )


{-| On URL changes, the `Model`'s `Route` field is updated, the `UIState` and
Page Title are modified, & any relevant data is fetched from the backend.
-}
urlUpdate : Result String Route -> Model -> ( Model, Cmd Msg )
urlUpdate result model =
    let
        updatedModel =
            { model | route = Routing.routeFromResult result }
    in
        ( { updatedModel
            | uiState = updateUI updatedModel.route model.uiState
          }
        , Cmd.batch
            [ fetchForRoute updatedModel.route
            , setPageTitle updatedModel.route
            ]
        )


{-| The `main` function assembles the initial `Model`, `View.view`, and
`Update.update` functions to generate an Elm `Program` that takes no flags.
-}
main : Program Never
main =
    Navigation.program Routing.parser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }
