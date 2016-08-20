module Models exposing (..)

import Categories.Models exposing (Category)


type alias Model =
    { categories : List Category
    }


initialModel : Model
initialModel =
    { categories =
        [ Category 1 "Bush Beans" "" Nothing
        , Category 2 "Baked Beans" "" (Just 1)
        ]
    }
