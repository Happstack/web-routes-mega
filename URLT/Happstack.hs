{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, UndecidableInstances #-}
module URLT.Happstack where

import Control.Monad.Trans (lift)
import Happstack.Server (WebMonad(..))
import URLT (URLT)

instance (WebMonad a m) => WebMonad a (URLT url m) where
    finishWith a = lift $ finishWith a
