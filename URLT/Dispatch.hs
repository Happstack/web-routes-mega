{-# LANGUAGE TypeFamilies, FlexibleContexts #-}
module URLT.Dispatch where

import Network.Wai
import URLT.Wai
import URLT.TH

class Dispatch a where
  type Routes a
  type App a
  dispatch :: a -> (Routes a -> String) -> Routes a -> (App a)

handleWaiD :: (Dispatch a, AsURL (Routes a), App a ~ Application) => a -> String -> Application
handleWaiD args approot = handleWaiU (dispatch args) approot