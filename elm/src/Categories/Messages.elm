module Categories.Messages exposing (..)

import HttpBuilder

import Api.Models exposing (Category, CategoryId)
import Categories.Models exposing (CategoryData)


type Msg
    = FetchAllDone CategoryData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone Category
    | FetchOneFail (HttpBuilder.Error String)
    | VisitCategory CategoryId
