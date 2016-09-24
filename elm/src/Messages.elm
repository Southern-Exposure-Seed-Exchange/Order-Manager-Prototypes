module Messages exposing (..)

import Categories.Messages
import Products.Messages
import NavBar


type Msg
    = CategoriesMsg Categories.Messages.Msg
    | ProductsMsg Products.Messages.Msg
    | NavBarMessage NavBar.Msg
