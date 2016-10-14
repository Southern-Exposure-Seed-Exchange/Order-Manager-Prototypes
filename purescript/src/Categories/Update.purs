module Categories.Update where


import Data.Either (Either(..))
import Pux (EffModel, noEffects)

import Categories.Messages (Msg(..))
import Categories.Models (CategoryData)


update :: forall e. Msg -> CategoryData -> EffModel CategoryData Msg e
update (ReceiveCategories (Left _)) model =
    noEffects model
update (ReceiveCategories (Right newData)) _ =
    noEffects newData
update (ReceiveCategory (Left _)) model =
    noEffects model
update (ReceiveCategory (Right newData)) _ =
    noEffects newData
