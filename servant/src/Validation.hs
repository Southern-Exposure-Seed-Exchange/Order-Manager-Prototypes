{-# LANGUAGE OverloadedStrings #-}
module Validation where

import Control.Monad.Trans.Either   (left, EitherT)
import Data.Validation
import Data.Aeson
import Servant

import Types


type ModelValidation a = Validation' [Value] a

class Validated a where
        -- | The validate function defines the validation function for the
        -- instance. It should either return the error, or a list of 
        validate :: a -> ModelValidation a

        runValidate :: a -> AppM a
        --runValidate item = do
        --    validation <- validate item
        --    case validation of
        --        Left vs     -> left $ err400 { errBody = encode $ object ["errors" .= vs] }
        --        Right valid -> return valid
