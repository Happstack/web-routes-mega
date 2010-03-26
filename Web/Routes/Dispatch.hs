{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module Web.Routes.Dispatch where

import Network.Wai
import Web.Routes.PathInfo
import Web.Routes.Wai

class Dispatch a where
  type Routes a
  type App a
  dispatch :: a -> (Routes a -> String) -> Routes a -> (App a)

handleWaiD :: (Dispatch a, PathInfo (Routes a), App a ~ Application) => String -> a -> Application
handleWaiD approot args = handleWai approot (dispatch args) 