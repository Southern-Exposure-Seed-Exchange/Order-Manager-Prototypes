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


class SubModel a where
    updateModel :: Model -> a -> Model
    fromModel :: Model -> a
