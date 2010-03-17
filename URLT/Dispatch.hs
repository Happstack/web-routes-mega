{-# LANGUAGE TypeFamilies #-}
module URLT.Dispatch where

import Network.Wai

class Dispatch a where
  type Routes a
  type App a
  dispatch :: a -> (Routes a -> String) -> Routes a -> (App a)

