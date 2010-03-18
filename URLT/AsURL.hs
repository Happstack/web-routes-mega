module URLT.AsURL where

import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer, next, runConsumer)

class AsURL a where
    toURLS :: a -> ShowS
    fromURLC :: Consumer String (Failing a)

toURL :: (AsURL a) => a -> String
toURL u = '/' : toURLS u ""

fromURL :: (AsURL a) => String -> Failing a
fromURL str =
    fst $ runConsumer (words $ map (\c -> if c == '/' then ' ' else c) str) fromURLC
