module Categories.Messages exposing (..)

import HttpBuilder

import Categories.Models exposing (CategoryData, Category, CategoryId)


type Msg
    = FetchAllDone CategoryData
    | FetchAllFail (HttpBuilder.Error String)
    | FetchOneDone Category
    | FetchOneFail (HttpBuilder.Error String)
    | VisitCategory CategoryId
