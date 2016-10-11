module Main where

import Prelude hiding (div)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Signal.Channel (CHANNEL)

import Pux (renderToDOM, fromSimple, start)
import Pux.Html (Html, text, div, h1, p)


data Msg
    = NoOp

type Model
    = Unit

update :: Msg -> Model -> Model
update NoOp = id

view :: Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Dashboard" ]
        , p [] [ text "Something Here Eventually" ]
        ]

main :: forall e. Eff (err :: EXCEPTION, channel :: CHANNEL | e) Unit
main = do
    app <- start
        { initialState: unit
        , update: fromSimple update
        , view
        , inputs: []
        }
    renderToDOM "#app" app.html
