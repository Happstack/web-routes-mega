{-# LANGUAGE DeriveFunctor, DeriveDataTypeable, FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving, ScopedTypeVariables, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, TypeOperators #-}
module Text.Zwaluw.Core 
{-
    ( -- * Types
    RouteError(..), Router(..), (:-)(..), (.~)
    -- * Running routers
    , parse, unparse
    -- * Constructing / Manipulating Routers
    , duck, eor, lit, printAs, pure, routeError, val, xmap
    -- heterogeneous list functions
    , hhead, htail, hdMap, hdTraverse, pop, arg, xmaph
    ) -} where

import Prelude          hiding ((.), id, (/))
import Control.Applicative (Applicative(..))
import Control.Arrow    (first, second)
import Control.Category (Category((.), id))
import Control.Monad    (MonadPlus(mzero, mplus))
import Control.Monad.Error
import Control.Monad.List
import Control.Monad.State
import Control.Monad.Trans
import Data.Data        (Data, Typeable)
import Data.Either      (partitionEithers)
import Data.Function    (on)
import Data.List        (stripPrefix)
import Data.Monoid      (Monoid(mappend, mempty))
import Data.String      (IsString(..))
import Text.Zwaluw.HList
import Text.Zwaluw.Pos

compose
  :: (a -> b -> c)
  -> (i -> [(a, j)])
  -> (j -> [(b, k)])
  -> (i -> [(c, k)])
compose op mf mg s = do
  (f, s') <- mf s
  (g, s'') <- mg s'
  return (f `op` g, s'')

-- | The 'maximumsBy' function takes a comparison function and a list
-- and returns the greatest elements of the list by the comparison function.
-- The list must be finite and non-empty.
maximumsBy               :: (a -> a -> Ordering) -> [a] -> [a]
maximumsBy _ []          =  error "Text.Zwaluw.Core.maximumsBy: empty list"
maximumsBy cmp (x:xs)        =  foldl maxBy [x] xs
                        where
                           maxBy xs@(x:_) y = case cmp x y of
                                       GT -> xs
                                       EQ -> (y:xs)
                                       LT  -> [y]

newtype Parser e tok a = Parser { runParser :: tok -> Pos e -> [Either e ((a, tok), Pos e)] }

instance Functor (Parser e tok) where
    fmap f (Parser p) = 
        Parser $ \tok pos ->
            map (fmap (first (first f))) (p tok pos)

instance Monad (Parser e tok) where
    return a = 
        Parser $ \tok pos ->
            [Right ((a, tok), pos)]
    (Parser p) >>= f =
        Parser $ \tok pos ->
            case partitionEithers (p tok pos) of
              ([], []) -> []
              (errs,[]) -> map Left errs
              (_,as) -> concat [ runParser (f a) tok' pos' | ((a, tok'), pos') <- as ]

instance MonadPlus (Parser e tok) where
    mzero = Parser $ \tok pos -> []
    (Parser x) `mplus` (Parser y) =
        Parser $ \tok pos ->
            (x tok pos) ++ (y tok pos)

composeP
  :: (a -> b -> c)
  -> Parser e tok a
  -> Parser e tok b
  -> Parser e tok c
composeP op mf mg = 
    do f <- mf
       g <- mg
       return (f `op` g)
-- | A @Router a b@ takes an @a@ to parse a URL and results in @b@ if parsing succeeds.
--   And it takes a @b@ to serialize to a URL and results in @a@ if serializing succeeds.
data Router e tok a b = Router
  { prs :: Parser e tok (a -> b)
  , ser :: b -> [(tok -> tok, a)]
  }

instance Category (Router e tok) where
  id = Router
    (return id)
    (\x -> [(id, x)])

  ~(Router pf sf) . ~(Router pg sg) = Router 
    (composeP (.) pf pg)
    (compose (.) sf sg) 


infixr 9 .~
-- | Reverse composition, but with the side effects still in left-to-right order.
(.~) :: Router e tok a b -> Router e tok b c -> Router e tok a c
~(Router pf sf) .~ ~(Router pg sg) = Router 
  (composeP (flip (.)) pf pg)
  (compose (flip (.)) sg sf)

instance Monoid (Router e tok a b) where
  mempty = Router 
    mzero
    (const mzero)

  ~(Router pf sf) `mappend` ~(Router pg sg) = Router 
    (pf `mplus` pg)
    (\s -> sf s `mplus` sg s)

-- | Map over routers.
xmap :: (a -> b) -> (b -> Maybe a) -> Router e tok r a -> Router e tok r b
xmap f g (Router p s) = Router p' s'
    where
      p' = fmap (fmap f) p
      s' url = maybe mzero s (g url)

-- | Lift a constructor-destructor pair to a pure router.
xpure :: (a -> b) -> (b -> Maybe a) -> Router e tok a b
xpure f g = xmap f g id

-- | Like "xmap", but only maps over the top of the stack.
xmaph :: (a -> b) -> (b -> Maybe a) -> Router e tok i (a :- o) -> Router e tok i (b :- o)
xmaph f g = xmap (hdMap f) (hdTraverse g)

val :: forall e tok a r. Parser e tok a -> (a -> [tok -> tok]) -> Router e tok r (a :- r)
val rs ss = Router rs' ss'
    where
      rs' :: Parser e tok (r -> (a :- r))
      rs' = fmap (:-) rs
      ss' =  (\(a :- r) -> map (\f -> (f, r)) (ss a))

-- | Give all possible parses or errors.
parse :: (Position (Pos e)) => Router e tok () a -> tok -> [Either e (a, tok)]
parse p s = 
    map (either Left (\((f, tok), _) -> Right (f (), tok))) $ runParser (prs p) s initialPos

bestErrors :: (ErrorPosition e, Ord (Pos e)) => [e] -> [e]
bestErrors [] = []
bestErrors errs = maximumsBy (compare `on` getPosition) errs

-- | Give the first parse, for Routers with a parser that yields just one value. 
-- Otherwise return the error (or errors) with the highest error position.
parse1 :: (ErrorPosition e, Position (Pos e), Show e, Ord (Pos e)) =>
     (tok -> Bool) -> Router e tok () (a :- ()) -> tok -> Either [e] a
parse1 isComplete r paths = 
    let results = parse r paths
    in case [ a | (Right (a,tok)) <- results, isComplete tok ] of
         ((u :- ()):_) -> Right u
         _             -> Left $ bestErrors [ e | Left e <- results ]


-- | Give all possible serializations.
unparse :: tok -> Router e tok () url -> url -> [tok]
unparse tok p = (map (($ tok) . fst)) . ser p


-- | Give the first serialization, for Routers with a serializer that needs just one value.
unparse1 :: (Error e) => tok -> Router e tok () (a :- ()) -> a -> Maybe tok
unparse1 tok p a = 
    case unparse tok p (a :- ()) of
      [] -> Nothing
      (s:_) -> Just s
