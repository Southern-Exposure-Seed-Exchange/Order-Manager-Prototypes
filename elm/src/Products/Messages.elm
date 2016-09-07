module Products.Messages exposing (..)

import HttpBuilder

import Api.Models exposing (Product, ProductId)
import Products.Models exposing (ProductData)


type Msg
    = FetchAllDone ProductData
    | FetchAllFail (HttpBuilder.Error String)
