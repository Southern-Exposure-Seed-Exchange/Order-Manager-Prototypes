module Products.Models where

import Data.List (List)

import Api.Models (Category, Product, ProductVariant)
import Classes (class SubModel)


data ProductData = ProductData
    { products :: List Product
    , variants :: List ProductVariant
    , categories :: List Category
    }


instance subModelProductData :: SubModel ProductData where
    updateModel model (ProductData { products, variants, categories }) =
        model { products = products, variants = variants, categories = categories }
    fromModel { products, variants, categories } =
        ProductData { products, variants, categories }
