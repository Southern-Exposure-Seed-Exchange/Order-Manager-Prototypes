module App (omApp) where

import Web.Spock.Safe           (subcomponent)

import Handlers.Categories
import Handlers.Products
import Types

omApp :: OM a
omApp = do
    subcomponent "categories" categoryHandlers
    subcomponent "products" productHandlers
