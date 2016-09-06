module Messages exposing (..)

import Categories.Messages
import Routing exposing (Route)


type Msg
    = CategoriesMsg Categories.Messages.Msg
    | RoutingMsg Route
