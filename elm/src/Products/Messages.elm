module Products.Messages exposing (..)

import HttpBuilder
import Api.Models exposing (Product, ProductId, CategoryId)
import Products.Form
import Products.Models exposing (ProductData)


type Msg
    = FetchAllDone ProductData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone ProductId ProductData
    | FetchOneFail (HttpBuilder.Error String)
    | DeleteOneDone ProductId
    | DeleteOneFail (HttpBuilder.Error String)
    | ToggleSKUs ProductId
    | ToggleAllSKUs
    | VisitProduct ProductId
    | VisitCategory CategoryId
    | AddProduct
    | EditProduct ProductId
    | DeleteProduct ProductId
    | ProductDeletionConfirmed ProductId
    | FormMessage Products.Form.Msg
