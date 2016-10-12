module Model where

import Data.List (List(Nil))

import Api.Models (Category, Product)


type Model =
    { categories :: List Category
    , products :: List Product
    }


init :: Model
init =
    { categories: Nil
    , products: Nil
    }
