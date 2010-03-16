{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module URLT.Regular where

import Control.Applicative
import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer(Consumer), next, runConsumer)
import Control.Monad(MonadPlus(mzero, mplus), ap)
import Data.Char (toLower)
import Generics.Regular
import URLT.TH (AsURL(fromURLC, toURLS))

class GToURL f where
   gtoURLS   :: f a -> ShowS
   gfromURLC :: Consumer String (Failing (f a))
   
instance AsURL a => GToURL (K a) where   
  gtoURLS (K a) = toURLS a
  gfromURLC = fmap (fmap K) $ fromURLC 
  
instance (GToURL f, GToURL g) => GToURL (f :+: g) where
   gtoURLS   (L x) = gtoURLS x
   gtoURLS   (R y) = gtoURLS y
   gfromURLC = let urlLeft  = fmap (fmap L) $ gfromURLC
                   urlRight = fmap (fmap R) $ gfromURLC
              in urlLeft `combine` urlRight
                where
                  combine :: Consumer String (Failing a) -> Consumer String (Failing a) -> Consumer String (Failing a)
                  combine (Consumer f) (Consumer g) =
                    Consumer $ \c ->
                    case f c of
                      r@(Success a, _) -> r
                      (Failure errs1, _) ->
                        case g c of
                          r@(Success a, _) -> r
                          (Failure errs2, _) -> (Failure (errs1 ++ errs2), c)

instance GToURL U where
  gtoURLS U = id
  gfromURLC =
    do m <- next
       case m of
         Nothing -> return (Success U)
         (Just str) -> return (Failure ["Excepted end of input, but got: " ++ str])

instance GToURL f => GToURL (S s f) where
   gtoURLS   (S x) = gtoURLS x
   gfromURLC       = fmap (fmap S)  gfromURLC
   
instance forall c f. (Constructor c, GToURL f) => GToURL (C c f) where
   gtoURLS c@(C x)  = showString (lower $ conName c) . showString "/" . gtoURLS x
   gfromURLC = let constr = undefined :: C c f r
                   name   = conName constr
               in do mx <- next
                     case mx of  
                       Nothing -> return (Failure ["Excepted '" ++ lower name ++ "' but got end of input."])
                       (Just x) ->
                         if   (lower x == lower name)
                         then fmap (fmap C) $ gfromURLC
                         else return (Failure ["Excepted '" ++ lower name ++ "' but got '" ++ lower x ++ "'"])

lower = map toLower
