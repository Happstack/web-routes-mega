{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Zwaluw.Regular 
  ( mkRouters
  , Routers
  , RouterList(..)
  
  -- * Re-exported from Generics.Regular
  , deriveAll
  , PF
  ) where

import Text.Zwaluw.Core
import Generics.Regular

-- infixr :&


-- | The type of the list of routers generated for type @r@.
type Routers e r = RouterList e (PF r) r

-- | Creates the routers for type @r@, one for each constructor. For example:
--
--   @Z rHome :& Z rUserOverview :& Z rUserDetail :& Z rArticle = mkRouters@
mkRouters :: (MkRouters (PF r), Regular r) => Routers e r
mkRouters = mkRouters' to (Right . from)

data family RouterList e f r
class MkRouters (f :: * -> *) where
  mkRouters' :: (f r -> r) -> (r -> Either [e] (f r)) -> RouterList e f r

data instance RouterList e (C c f) r = Z (forall t. Router e (RouterLhs f r t) (r :- t))
instance MkRouter f => MkRouters (C c f) where
  mkRouters' addLR matchLR = Z $ pure (hdMap (addLR . C) . mkP) (fmap mkS . hdTraverse (fmap unC . matchLR))

data instance RouterList e (f :+: g) r = RouterList e f r :& RouterList e g r
instance (MkRouters f, MkRouters g) => MkRouters (f :+: g) where
  mkRouters' addLR matchLR = mkRouters' (addLR . L) (matchL matchLR) 
                          :& mkRouters' (addLR . R) (matchR matchLR)
    where
      matchL :: (r -> Either [e] ((f :+: g) r)) -> r -> Either [e] (f r)
      matchL frm r = case frm r of 
        Right (L f) -> Right f
--         _ -> Nothing

      matchR :: (r -> Either [e] ((f :+: g) r)) -> r -> Either [e] (g r)
      matchR frm r = case frm r of 
        Right (R f) -> Right f
--        _ -> Nothing


type family RouterLhs (f :: * -> *) (r :: *) (t :: *) :: *
class MkRouter (f :: * -> *) where
  mkP :: RouterLhs f r t -> (f r :- t)
  mkS :: (f r :- t) -> RouterLhs f r t

type instance RouterLhs U r t = t
instance MkRouter U where
  mkP t = U :- t
  mkS (U :- r) = r

type instance RouterLhs (K a) r t = a :- t
instance MkRouter (K a) where
  mkP (a :- t) = K a :- t
  mkS (K a :- t) = a :- t

type instance RouterLhs I r t = r :- t
instance MkRouter I where
  mkP (r :- t) = I r :- t
  mkS (I r :- t) = r :- t

type instance RouterLhs (f :*: g) r t = RouterLhs f r (RouterLhs g r t)
instance (MkRouter f, MkRouter g) => MkRouter (f :*: g) where
  mkP t = (f :*: g) :- t''
    where 
      f :- t'  = mkP t
      g :- t'' = mkP t'
  mkS ((f :*: g) :- t) = mkS (f :- mkS (g :- t))
