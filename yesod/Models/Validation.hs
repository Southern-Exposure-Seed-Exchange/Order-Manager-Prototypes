module Models.Validation where

import Foundation
import Import.NoFoundation


class Validation a where
        validate :: a -> Handler (Either Value a)

instance Validation Category where
        validate category = do
            entities <- runDB $ selectList [] []
            let categories = map (\(Entity _ c) -> c) entities
            let isUnique = not $ any (\c -> categoryName c == categoryName category) categories
            return $ if isUnique
                        then Right category
                        else Left $ mkError "name" "Unique Attribute"
                                            "The category name must be unique."

mkError :: Text -> Text -> Text -> Value
mkError field title detail = object
        [ "source" .= object ["pointer" .= ("/data/attributes/" <> field :: Text)]
        , "title"  .= title
        , "detail" .= detail
        ]
