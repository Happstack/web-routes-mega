{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module URLT.Dispatch where

import Network.Wai
import URLT.PathInfo
import URLT.Wai

class Dispatch a where
  type Routes a
  type App a
  dispatch :: a -> (Routes a -> String) -> Routes a -> (App a)

handleWaiD :: (Dispatch a, PathInfo (Routes a), App a ~ Application) => String -> a -> Application
handleWaiD approot args = handleWai approot (dispatch args) 