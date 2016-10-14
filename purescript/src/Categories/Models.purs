module Categories.Models where

import Data.List (List)

import Api.Models (Category, Product)
import Classes (class SubModel)
import Utils (replaceAll)


data CategoryData = CategoryData
    { categories :: List Category
    , products :: List Product
    }


instance submodelCategoryData :: SubModel CategoryData where
    updateModel model (CategoryData { categories, products }) =
        model { categories = replaceAll model.categories categories
              , products = replaceAll model.products products }
    fromModel { categories, products } =
        CategoryData { categories, products }
