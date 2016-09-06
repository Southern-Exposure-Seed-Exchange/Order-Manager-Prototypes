module Models exposing (..)

import Categories.Models exposing (CategoryData)


type alias Model =
    CategoryData


initialModel : Model
initialModel =
    { categories = []
    , products = []
    }
