module Categories.Messages exposing (..)

import HttpBuilder

import Api.Models exposing (Category, CategoryId, ProductId)
import Categories.Models exposing (CategoryData)


type Msg
    = FetchAllDone CategoryData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone CategoryId CategoryData
    | FetchOneFail (HttpBuilder.Error String)
    | UpdateOneDone CategoryId Category
    | UpdateOneFail (HttpBuilder.Error String)
    | VisitCategory CategoryId
    | EditCategory CategoryId
    | VisitProduct ProductId
    | FormNameChange String
    | FormDescriptionChange String
    | FormParentChange String
    | SaveForm
    | ResetForm
    | CancelForm
