module Main where

import Prelude (Unit, bind)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Network.HTTP.Affjax (AJAX)
import Pux (renderToDOM, start)
import Signal.Channel (CHANNEL)

import Model (init)
import Update (update)
import View (view)


main :: Eff (err :: EXCEPTION, channel :: CHANNEL, ajax :: AJAX) Unit
main = do
    app <- start
        { initialState: init
        , update
        , view
        , inputs: []
        }
    renderToDOM "#app" app.html
