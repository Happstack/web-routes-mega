{-# LANGUAGE TypeOperators, ScopedTypeVariables #-}
module URLT.Regular where

import Control.Applicative
import Control.Applicative.Error (Failing(Failure, Success))
import Control.Monad.Consumer (Consumer(Consumer), next, runConsumer)
import Control.Monad(MonadPlus(mzero, mplus), ap)
import Data.Char (toLower)
import Generics.Regular
import URLT.PathInfo (PathInfo(fromPathSegments, toPathSegments))

class GToURL f where
   gtoPathSegments   :: f a -> [String]
   gfromPathSegments :: Consumer String (Failing (f a))

instance PathInfo a => GToURL (K a) where   
  gtoPathSegments (K a) = toPathSegments a
  gfromPathSegments = fmap (fmap K) $ fromPathSegments 

instance (GToURL f, GToURL g) => GToURL (f :+: g) where
   gtoPathSegments   (L x) = gtoPathSegments x
   gtoPathSegments   (R y) = gtoPathSegments y
   gfromPathSegments = 
     let urlLeft  = fmap (fmap L) $ gfromPathSegments
         urlRight = fmap (fmap R) $ gfromPathSegments
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
  gtoPathSegments U = []
  gfromPathSegments =
    do m <- next
       case m of
         Nothing -> return (Success U)
         (Just str) -> return (Failure ["Excepted end of input, but got: " ++ str])

instance GToURL f => GToURL (S s f) where
   gtoPathSegments   (S x) = gtoPathSegments x
   gfromPathSegments       = fmap (fmap S)  gfromPathSegments

lower :: String -> String
lower = map toLower

instance forall c f. (Constructor c, GToURL f) => GToURL (C c f) where
   gtoPathSegments c@(C x)  = (lower $ conName c) : gtoPathSegments x
   gfromPathSegments = 
     let constr = undefined :: C c f r
         name   = conName constr
     in do mx <- next
           case mx of  
             Nothing -> return (Failure ["Excepted '" ++ lower name ++ "' but got end of input."])
             (Just x) ->
               if   (lower x == lower name)
               then fmap (fmap C) $ gfromPathSegments
               else return (Failure ["Excepted '" ++ lower name ++ "' but got '" ++ lower x ++ "'"])
