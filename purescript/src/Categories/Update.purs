module Categories.Update where


import Prelude
import Data.Either (Either(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

import Categories.Messages (Msg(..))
import Categories.Commands (fetchCategories)
import Model (Model)

update :: Msg -> Model -> EffModel Model Msg (ajax :: AJAX)
update FetchCategories model =
    { state: model, effects: [ fetchCategories ] }
update (ReceiveCategories (Left _)) model =
    noEffects $ model
update (ReceiveCategories (Right categories)) model =
    noEffects $ model { categories = categories }
