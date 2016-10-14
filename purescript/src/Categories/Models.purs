module Categories.Models where

import Prelude (($))
import Data.List (List)

import Api.Models (Category, Product)
import Classes (class SubModel)


data CategoryData = CategoryData
    { categories :: List Category
    , products :: List Product
    }


instance submodelCategoryData :: SubModel CategoryData where
    updateModel model (CategoryData { categories, products }) =
        model { categories = categories, products = products }
    fromModel { categories, products } =
        CategoryData { categories, products }
