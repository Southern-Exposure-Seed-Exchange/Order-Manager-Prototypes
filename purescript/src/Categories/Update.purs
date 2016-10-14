module Categories.Update where


import Prelude (($), (<<<), map)
import Control.Monad.Aff (apathize)
import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import DOM (DOM)
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects, onlyEffects)
import Pux.Router (navigateTo)

import Api.Models (Category(..), Product(..))
import Categories.Commands (deleteCategory)
import Categories.Messages (Msg(..))
import Categories.Models (CategoryData(..))
import Utils (deleteById, replaceAll, sortByField)


update :: Msg -> CategoryData -> EffModel CategoryData Msg ( ajax :: AJAX, dom :: DOM )
update (DeleteCategory id) model =
    onlyEffects model [ deleteCategory id ]
update (ReceiveCategories (Left _)) model =
    noEffects model
update (ReceiveCategories (Right newData)) model =
    noEffects $ updateCategoryData model newData
update (ReceiveCategory (Left _)) model =
    noEffects model
update (ReceiveCategory (Right newData)) model =
    noEffects $ updateCategoryData model newData
update (DeletedCategory (Left _)) model =
    noEffects model
update (DeletedCategory (Right id)) (CategoryData model) =
    { state: CategoryData $ model { categories = deleteById id model.categories }
    , effects: [ map NoOp <<< apathize <<< liftEff $ navigateTo "/categories" ]
    }
update (NoOp _) model =
    noEffects model


-- | Update the main CategoryData value using a fetched & decoded CategoryData.
updateCategoryData :: CategoryData -> CategoryData -> CategoryData
updateCategoryData (CategoryData oldData) (CategoryData newData) =
    CategoryData $ oldData
        { categories = sortByField (\(Category c) -> c.name) $
                       replaceAll oldData.categories newData.categories
        , products = sortByField (\(Product p) -> p.name) $
                     replaceAll oldData.products newData.products
        }
