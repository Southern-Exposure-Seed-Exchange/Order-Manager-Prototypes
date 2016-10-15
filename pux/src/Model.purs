module Model where

import Data.List (List(Nil))

import Api.Models (Category, Product, ProductVariant)
import Router (Route(Home))


type Model =
    { categories :: List Category
    , products :: List Product
    , variants :: List ProductVariant
    , route :: Route
    }


init :: Model
init =
    { categories: Nil
    , products: Nil
    , variants: Nil
    , route: Home
    }
