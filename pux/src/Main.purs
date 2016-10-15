module Main where

import Prelude (Unit, bind, (<<<))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Pux (renderToDOM, start)
import Pux.Router (sampleUrl)
import Signal ((~>))
import Signal.Channel (CHANNEL)

import Messages (Msg(PageView))
import Model (init)
import Router (match)
import Update (update)
import View (view)


main :: Eff (ajax :: AJAX, channel :: CHANNEL, dom :: DOM, err :: EXCEPTION) Unit
main = do
    urlSignal <- sampleUrl
    let routeSignal = urlSignal ~> (PageView <<< match)
    app <- start
        { initialState: init
        , update
        , view
        , inputs: [routeSignal]
        }
    renderToDOM "#app" app.html
