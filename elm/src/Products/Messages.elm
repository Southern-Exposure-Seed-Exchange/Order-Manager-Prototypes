module Products.Messages exposing (..)

import HttpBuilder
import Api.Models exposing (Product, ProductId, CategoryId)
import Products.Models exposing (ProductData)


type Msg
    = FetchAllDone ProductData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone ProductData
    | FetchOneFail (HttpBuilder.Error String)
    | ToggleSKUs ProductId
    | ToggleAllSKUs
    | VisitProduct ProductId
    | VisitCategory CategoryId
