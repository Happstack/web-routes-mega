{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TypeFamilies, TypeOperators #-}
module Web.Routes.Zwaluw.Core 
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
import Control.Arrow    (first, second)
import Control.Category (Category((.), id))
import Control.Monad    (MonadPlus(mzero, mplus))
import Control.Monad.Trans.Error (Error(..), ErrorList(..))
import Data.Data        (Data, Typeable)
import Data.Either      (partitionEithers)
import Data.List        (stripPrefix)
import Data.Monoid      (Monoid(mappend, mempty))
import Data.String      (IsString(..))

infixr 8 :-
infixr 9 .~

compose
  :: (a -> b -> c)
  -> (i -> Either e [(a, j)])
  -> (j -> Either e [(b, k)])
  -> (i -> Either e [(c, k)])
compose op mf mg = \s ->
  case mf s of
    (Left e) -> (Left e)
    (Right fs) ->
        case partitionEithers [ fmap (map (first (op f))) (mg s') | (f, s') <- fs  ] of
          ([], []) -> Right []
          (errs,[]) -> Left (last errs)
          (_, succs) -> Right (concat succs)


bind :: Either e [a] -> (a -> Either e [b]) -> Either e [b]
bind (Left e) _ = (Left e)
bind (Right l) f =
    case partitionEithers (map f l) of
      ([], []) -> Right []
      (errs,[]) -> Left (last errs)
      (_, succ) -> Right (concat succ)

-- | A @Router a b@ takes an @a@ to parse a URL and results in @b@ if parsing succeeds.
--   And it takes a @b@ to serialize to a URL and results in @a@ if serializing succeeds.
data Router e a b = Router
  { prs :: [String] -> Either e [(a -> b, [String])]
  , ser :: b -> Either e [([String] -> [String], a)]
  }

mapRouteError :: (e -> e') -> Router e a b -> Router e' a b
mapRouteError f (Router pf sf) =
    Router (\a -> either (Left . f) (Right . id) (pf a))
           (\b -> either (Left . f) (Right . id) (sf b))

instance Category (Router e) where
  id = Router
    (\x -> return [(id, x)])
    (\x -> return [(id, x)])

  ~(Router pf sf) . ~(Router pg sg) = Router 
    (compose (.) pf pg)
    (compose (.) sf sg) 

-- | Reverse composition, but with the side effects still in left-to-right order.
(.~) :: Router e a b -> Router e b c -> Router e a c
~(Router pf sf) .~ ~(Router pg sg) = Router 
  (compose (flip (.)) pf pg)
  (compose (flip (.)) sg sf)

instance (Error e) => Monoid (Router e a b) where
  mempty = Router 
    (const mzero)
    (const mzero)
  ~(Router pf sf) `mappend` ~(Router pg sg) = Router 
    (\s -> pf s `plus` pg s)
    (\s -> sf s `plus` sg s)

plus :: Either e [a] -> Either e [a] -> Either e [a]
plus (Right x) (Right y) = Right (x ++ y)
plus (Right x) _ = Right x
plus _ a = a

data RouteError 
    = InvalidLit String String
    | RouteEOF
    | Other String
      deriving (Eq, Ord, Read, Show, Typeable, Data)

instance ErrorList RouteError where
    listMsg s = [Other s]

instance Error RouteError where
    strMsg s = Other s

routeError :: e -> Either e b
routeError e = Left e

instance a ~ b => IsString (Router RouteError a b) where
  fromString = lit

-- | Routes a constant string.
lit :: String -> Router RouteError r r
lit l = Router pf sf 
    where
      pf (p:ps) =
          case stripPrefix l p of
            (Just p') -> return [(id, p':ps)]
            Nothing -> routeError (InvalidLit p l)
      pf [] = routeError RouteEOF
      sf b = return [(( \(s:ss) -> ((l ++ s) : ss)), b)]
{-
eor :: Router RouteError r r
eor = Router
      (\path ->
           case path of
             []   -> return (id, [])
             [""] -> return (id, [])
             o    -> Left $ listMsg $ "left-over data: " ++ show o)
      (\a -> return (id, a))
-}

eops :: (Error e) => Router e r r
eops = Router 
       (\path -> case path of
                   []      -> return   [(id, [])]
                   ("":ps) -> return [(id, ps)]
                   (p:_) -> Left $ strMsg $ "path-segment not entirely consumed: " ++ p)
       (\a -> return [(("" :), a)])
       

-- | Map over routers.
xmap :: (a -> b) -> (b -> Either e a) -> Router e r a -> Router e r b
xmap f g (Router p s) = Router p' s'
    where
      p' = (fmap . fmap . map . first . fmap) f p
      s' url = s =<< (g url)

-- | Lift a constructor-destructor pair to a pure router.
xpure :: (a -> b) -> (b -> Either e a) -> Router e a b
xpure f g = xmap f g id

-- | A stack datatype. Just a better looking tuple.
data a :- b = a :- b deriving (Eq, Show)

-- | Stack destructor.
pop :: (a -> b -> r) -> (a :- b) -> r
pop f (a :- b) = f a b

-- | Get the top of the stack.
hhead :: (a :- b) -> a
hhead (a :- _) = a

-- | Get the stack with the top popped.
htail :: (a :- b) -> b
htail (_ :- b) = b

-- | Applicative traversal over the top of the stack.
hdTraverse :: Functor f => (a -> f b) -> a :- t -> f (b :- t)
hdTraverse f (a :- t) = fmap (:- t) (f a)

arg :: (ty -> r -> s) -> (a -> ty) -> (a :- r) -> s
arg c f = pop (c . f)

-- | Map over the top of the stack.
hdMap :: (a1 -> a2) -> (a1 :- b) -> (a2 :- b)
hdMap = arg (:-)

-- | Like "xmap", but only maps over the top of the stack.
xmaph :: (a -> b) -> (b -> Either e a) -> Router e i (a :- o) -> Router e i (b :- o)
xmaph f g = xmap (hdMap f) (hdTraverse g)

-- | Build a router for a value given all the ways to parse and serialize it.
val :: ([String] ->  Either e [(a, [String])]) -> (a -> Either e [[String] -> [String]]) -> Router e r (a :- r)
val rs ss = Router
    (fmap (map (first (:-))) . rs)
    (\(a :- r) -> fmap (map (\f -> (f, r))) (ss a))

-- | Convert a router to do what it does on the tail of the stack.
duck :: Router e r1 r2 -> Router e (h :- r1) (h :- r2)
duck r = Router
  (fmap (map (first (\f (h :- t) -> h :- f t))) . prs r)
  (\(h :- t) -> fmap (map (second (h :-))) $ ser r t)

-- | Convert a router to do what it does on the tail of the stack.
duck1 :: Router e r1 (a :- r2) -> Router e (h :- r1) (a :- h :- r2)
duck1 r = Router
  (fmap (map (first (\f (h :- t) -> let a :- t' = f t in a :- h :- t'))) . prs r)
  (\(a :- h :- t) -> fmap (map (second (h :-))) $ ser r (a :- t))
{-
-- | @r \`printAs\` s@ uses the serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Router e a b -> [String] -> Router e a b
printAs r s = r { ser = ser' }
    where
      ser' url =
          case (ser r) url of
            Left e         -> Left e
            (Right (_, a)) -> Right (const s, a)
-}
            

-- | Give all possible parses.
parse :: Router e () a -> [String] -> Either e [a]
parse p s = 
    case prs p s of
      (Left errs)     -> Left errs
      (Right fs) -> Right [ f () | (f, r) <- fs, empty r ]
    where
      empty [] = True
      empty [[]] = True
      empty _ = False

-- | Give the first parse, for Routers with a parser that yields just one value.
parse1 :: (Error e) => Router e () (a :- ()) -> [String] -> Either e a
parse1 p s = 
    case parse p s of 
      (Left e) -> (Left e)
      (Right as) -> 
          case map hhead as of
            [] -> Left (strMsg "no complete parses.")
            (a:_) -> Right a
                      
-- | Give all possible serializations.
unparse :: Router e () url -> url -> Either e [[String]]
unparse p = fmap (map (($ [[]]) . fst)) . ser p

-- | Give the first serialization, for Routers with a serializer that needs just one value.
unparse1 :: (Error e) => Router e () (a :- ()) -> a -> Either e [String]
unparse1 p a = 
    case unparse p (a :- ()) of
      (Left e)   -> Left e
      (Right []) -> Left (strMsg "unparse1: no valid unparsing")
      (Right (s:_)) -> Right s
