module Models exposing (..)

import Api.Models exposing (Category, Product)
import Routing


type alias Model =
    { categories : List Category
    , products : List Product
    , route : Routing.Route
    }


initialModel : Routing.Route -> Model
initialModel route =
    { categories = []
    , products = []
    , route = route
    }
