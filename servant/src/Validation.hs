{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module Validation (Validation(..)) where

import Control.Monad.Trans.Class    (lift)
import Control.Monad.Trans.Except   (throwE)
import Data.Aeson
import Data.Maybe                   (isJust, fromJust, fromMaybe)
import Database.Persist             ( selectFirst, (!=.), (==.), Key
                                    , EntityField, PersistEntityBackend
                                    , PersistField, PersistEntity)
import Database.Persist.Sql         (SqlBackend)
import Servant
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import Types
import Models


-- | An HTTP 422 error is returned when a validation returns an error.
err422 :: ServantErr
err422 = err400 { errHTTPCode = 422
                , errReasonPhrase = "Unprocessable Entity"
                }

type FieldName = T.Text
type ErrorMessage = T.Text

-- | The Validated type represents a Model that has undergone validation.
-- The HashMap of errors should use field names as keys and a list of
-- errors for that field as values. An empty field name is used for errors
-- tht apply to the whole Model, not a particular field.
data Validated a = Validated (HM.HashMap FieldName [ErrorMessage], a)

-- | The Validation typeclass is responsible for performing extended
-- validation on new or updated entries.
class Validation a where
        -- | The validate function specifies the validation for the
        -- instance. It should return a `Validated` type containing
        -- a HashMap of errors and the post-validation instance state.
        validate :: Maybe (Key a) -> a -> AppM (Validated a)
        validate _ item = return $ Validated (HM.empty, item)

        -- | The runValidate function will validate an item and will either
        -- return the validated item or cause the server to return a 400
        -- error with a JSON body.
        runValidate :: Maybe (Key a) -> a -> AppM a
        runValidate key item = do
            Validated (errors, validatedItem) <- validate key item
            if HM.null errors
            then return validatedItem
            else lift . throwE $ err422
                    { errBody = encode $ object ["errors" .= jsonAPIErrors errors] }
            where -- | jsonAPIErrors takes a HashMap of Errors and returns
                  -- a list of JSON API errors.
                  jsonAPIErrors :: HM.HashMap FieldName [ErrorMessage] -> [Value]
                  jsonAPIErrors = map toJSON . HM.foldrWithKey (\k i list ->
                     map (fieldError k) i `mappend` list) []
                  -- | fieldError takes a FieldName & an ErrorMessage and
                  -- returns a single JSON API error. An empty FieldName is
                  -- assumed to refer to the entire object instead of
                  -- a single field.
                  fieldError :: FieldName -> ErrorMessage -> Value
                  fieldError field detail =
                    let pointer = if T.null field then "/data" else "/data/attributes/" `T.append` field
                    in object [ "detail" .= detail, "source" .= object ["pointer" .= pointer] ]


-- | A valid category should have a unique, non-empty name, and cannot be
-- it's own parent.
instance Validation Category where
        validate key cat = do
            let catName = categoryName cat
                parentIsSelf = fromMaybe False $ (==) <$> key <*> categoryParent cat
            sameName <- fieldValueExists CategoryId key CategoryName catName
            run cat [ ("name", [ ("The Name cannot be blank.", T.null catName)
                               , ("A Category with this name already exists.", sameName)
                               ])
                    , ("parent", [ ("A Category cannot be it's own Parent.", parentIsSelf)
                                 ])
                    ]

-- | A valid Product has a unique, non-empty name, & a valid Category.
instance Validation Product where
        validate key prod = do
            let prodName = productName prod
                catId = productCategory prod
            sameName <- fieldValueExists ProductId key ProductName prodName
            invalidCategory <- not <$> modelExists CategoryId catId
            run prod
                [ ("name", [ ("The Name cannot be blank.", T.null prodName)
                           , ("A Product with this name already exists.", sameName)
                           ])
                , ("category", [ ("The Category does not exist.", invalidCategory)
                               ])
                ]

-- | A valid ProductVariant has a unique, non-empty name, a positive
-- weight, & a valid Product.
instance Validation ProductVariant where
        validate key variant = do
            let sku = productVariantSku variant
                weight = productVariantWeight variant
                prodId = productVariantProduct variant
            sameName <- fieldValueExists ProductVariantId key ProductVariantSku sku
            invalidProduct <- not <$> modelExists ProductId prodId
            run variant
                [ ("sku", [ ("The SKU cannot be blank.", T.null sku)
                          , ("A Variant with this SKU already exists.", sameName)
                          ])
                , ("weight", [ ("The Weight cannot be negative.", isNegative weight)
                             ])
                , ("product", [ ("The Product does not exist.", invalidProduct)
                              ])
                ]



-- | Take an item and a list of potential errors & create a Validated item
-- containing only actual errors.
run :: a -> [(FieldName, [(ErrorMessage, Bool)])] -> AppM (Validated a)
run item errorPairs =
        return $ Validated ( HM.fromList . filter notEmpty $ map processErrors errorPairs
                           , item)
        where notEmpty (_, es) = not $ null es
              processErrors (field, errors) = (field, concatErrors errors)
              concatErrors = concatMap (\(message, hasError) -> [ message | hasError ])


-- | Determine whether or not a number is below zero.
isNegative :: (Ord a, Num a) => a -> Bool
isNegative = (< 0)

-- | Determine whether or not a model exists, given it's ID field & Id.
modelExists :: (PersistField (Key k), PersistEntity r, PersistEntityBackend r ~ SqlBackend)
            => EntityField r (Key k) -> Key k -> AppM Bool
modelExists idField = fieldValueExists idField Nothing idField

-- | Determine if a value already exists for a field. If a Key passed, the
-- respective entity is ignored.
fieldValueExists :: ( PersistEntityBackend r ~ SqlBackend, PersistEntity r
                    , PersistField key, PersistField f)
                 => EntityField r key -> Maybe key -> EntityField r f -> f -> AppM Bool
fieldValueExists idField key nameField nameValue = do
        sameName <- runDB $ flip selectFirst [] $ if isJust key
                                      then [ nameField ==. nameValue
                                           , idField !=. fromJust key ]
                                      else [ nameField ==. nameValue ]
        return $ isJust sameName
