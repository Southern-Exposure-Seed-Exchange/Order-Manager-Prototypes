module Messages exposing (..)

import Categories.Messages
import Products.Messages
import Routing exposing (Route)


type Msg
    = CategoriesMsg Categories.Messages.Msg
    | ProductsMsg Products.Messages.Msg
    | RoutingMsg Route
