{-# LANGUAGE TemplateHaskell, TypeOperators #-}
module Text.Zwaluw.Combinators where

import Control.Arrow
import Prelude hiding ((.), id, (/))
import Control.Category
import Control.Monad (guard)
import Control.Monad.Error (Error)
import Data.Monoid
import Text.Zwaluw.Core
import Text.Zwaluw.HList
import Text.Zwaluw.TH

infixr 8 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | Convert a router to do what it does on the tail of the stack.
duck :: Router e tok r1 r2 -> Router e tok (h :- r1) (h :- r2)
duck r = Router
  (fmap (\f (h :- t) -> h :- f t) $ prs r)
  (\(h :- t) -> map (second (h :-)) $ ser r t)

-- | Convert a router to do what it does on the tail of the stack.
duck1 :: Router e tok r1 (a :- r2) -> Router e tok (h :- r1) (a :- h :- r2)
duck1 r = Router
  (fmap (\f (h :- t) -> let a :- t' = f t in a :- h :- t') $ prs r)
  (\(a :- h :- t) -> map (second (h :-)) $ ser r (a :- t))

-- | Make a router optional.
opt :: Router e tok r r -> Router e tok r r
opt = (id <>)

manyr :: Router e tok r r -> Router e tok r r
manyr = opt . somer

-- | Repeat a router one or more times, combining the results from left to right.
somer :: Router e tok r r -> Router e tok r r
somer p = p . manyr p

-- | @chainr p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr :: Router e tok r r -> Router e tok r r -> Router e tok r r
chainr p op = opt (manyr (p .~ op) . p)

-- | @chainr1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr1 :: Router e tok r (a :- r) -> Router e tok (a :- a :- r) (a :- r) -> Router e tok r (a :- r)
chainr1 p op = manyr (duck p .~ op) . p

-- | Repeat a router zero or more times, combining the results from right to left.
manyl :: Router e tok r r -> Router e tok r r
manyl = opt . somel

-- | Repeat a router one or more times, combining the results from right to left.
somel :: Router e tok r r -> Router e tok r r
somel p = p .~ manyl p

-- | @chainl1 p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl :: Router e tok r r -> Router e tok r r -> Router e tok r r
chainl p op = opt (p .~ manyl (op . p))

-- | @chainl1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl1 :: Router e tok r (a :- r) -> Router e tok (a :- a :- r) (a :- r) -> Router e tok r (a :- r)
chainl1 p op = p .~ manyl (op . duck p)

{-
-- | Filtering on routers.
rFilter :: (a -> Bool) -> Router RouteError () (a :- ()) -> Router RouteError r (a :- r) 
rFilter p r = val ps ss
    where
      ps []    = routeError RouteEOF -- case (prs r) 
      ps paths =
          case prs r paths of
            (Left e) -> Left e
            (Right (f, paths')) ->
                let a = hhead (f ())
                in if (p a)
                   then (Right (a, paths'))
                   else (Left $ strMsg ("rFilter predicate failed."))
      ss url =
          case ser r (url :- ()) of
            (Left e) -> Left e
            (Right (f, _)) ->
                if p url
                then Right f
                else (Left $ strMsg ("rFilter predicate failed."))
-}
-- | Push a value on the stack (during parsing, pop it from the stack when serializing).
push :: (Eq a, Error e) => a -> Router e tok r (a :- r)
push a = xpure (a :-) (\(a' :- t) -> guard (a' == a) >> Just t)

rNil :: Router e tok r ([a] :- r)
rNil = xpure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: Router e tok (a :- [a] :- r) ([a] :- r)
rCons = xpure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList :: Router e tok r (a :- r) -> Router e tok r ([a] :- r)
rList r = manyr (rCons . duck1 r) . rNil

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList1 :: Router e tok r (a :- r) -> Router e tok r ([a] :- r)
rList1 r = somer (rCons . duck1 r) . rNil


-- | Converts a router for a value @a@ to a router for a list of @a@, with a separator.
rListSep :: Router e tok r (a :- r) -> Router e tok ([a] :- r) ([a] :- r) -> Router e tok r ([a] :- r)
rListSep r sep = chainr (rCons . duck1 r) sep . rNil

rPair :: Router e tok (f :- s :- r) ((f, s) :- r)
rPair = xpure (arg (arg (:-)) (,)) $ \(ab :- t) -> do (a,b) <- Just ab; Just (a :- b :- t)

$(deriveRouters ''Either)
rLeft  :: Router e tok (a :- r) (Either a b :- r)
rRight :: Router e tok (b :- r) (Either a b :- r)

-- | Combines a router for a value @a@ and a router for a value @b@ into a router for @Either a b@.
rEither :: Router e tok r (a :- r) -> Router e tok r (b :- r) -> Router e tok r (Either a b :- r)
rEither l r = rLeft . l <> rRight . r


$(deriveRouters ''Maybe)
rNothing :: Router e tok       r  (Maybe a :- r)
rJust    :: Router e tok (a :- r) (Maybe a :- r)

-- | Converts a router for a value @a@ to a router for a @Maybe a@.
rMaybe :: Router e tok r (a :- r) -> Router e tok r (Maybe a :- r)
rMaybe r = rJust . r <> rNothing

$(deriveRouters ''Bool)
rTrue  :: Router e tok r (Bool :- r)
rFalse :: Router e tok r (Bool :- r)

