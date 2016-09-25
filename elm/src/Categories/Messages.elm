module Categories.Messages exposing (..)

import HttpBuilder
import Api.Models exposing (Category, CategoryId, ProductId)
import Categories.Form
import Categories.Models exposing (CategoryData)


type Msg
    = FetchAllDone CategoryData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone CategoryId CategoryData
    | FetchOneFail (HttpBuilder.Error String)
    | DeleteOneDone CategoryId
    | DeleteOneFail (HttpBuilder.Error String)
    | AddCategory
    | VisitCategory CategoryId
    | EditCategory CategoryId
    | DeleteCategory CategoryId
    | VisitProduct ProductId
    | FormMessage Categories.Form.Msg
