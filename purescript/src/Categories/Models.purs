module Categories.Models where

import Prelude (($))
import Data.List (List)

import Api.Models (Category(..), Product(..))
import Classes (class SubModel)
import Utils (replaceAll, sortByField)


data CategoryData = CategoryData
    { categories :: List Category
    , products :: List Product
    }


instance submodelCategoryData :: SubModel CategoryData where
    updateModel model (CategoryData { categories, products }) =
        model { categories = sortByField (\(Category c) -> c.name) $
                             replaceAll model.categories categories
              , products = sortByField (\(Product p) -> p.name) $
                           replaceAll model.products products }
        where sortByName = sortByField (\x -> x.name :: String)
    fromModel { categories, products } =
        CategoryData { categories, products }
