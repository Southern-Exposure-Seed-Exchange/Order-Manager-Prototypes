module Model where

import Data.List (List(Nil))

import Api.Models (Category, Product)
import Router (Route(Home))


type Model =
    { categories :: List Category
    , products :: List Product
    , route :: Route
    }


init :: Model
init =
    { categories: Nil
    , products: Nil
    , route: Home
    }


class SubModel a where
    updateModel :: Model -> a -> Model
    fromModel :: Model -> a
