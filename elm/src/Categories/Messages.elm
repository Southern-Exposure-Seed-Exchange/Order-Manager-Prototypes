module Categories.Messages exposing (..)

import HttpBuilder

import Categories.Models exposing (CategoryData)


type Msg
    = FetchAllDone CategoryData
    | FetchAllFail (HttpBuilder.Error String)
