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
-- import Control.Monad.Trans.Error (Error(..), ErrorList(..))
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
maximumsBy _ []          =  error "List.maximumBy: empty list"
maximumsBy cmp (x:xs)        =  foldl maxBy [x] xs
                        where
                           maxBy xs@(x:_) y = case cmp x y of
                                       GT -> xs
                                       EQ -> (y:xs)
                                       LT  -> [y]

-- newtype Parser e tok a = Parser { runParser :: tok -> ListT (StateT (Pos e) (Either e)) (a, tok) }
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
{-
    case partitionEithers $ runParser (prs p) s initialPos of
      ([], [])   -> Right []
      (errs, []) -> Left $ errs
      (_, fs)    -> Right [ (f (), tok) | ((f, tok),_) <- fs ]
-}

{-
parse :: (Position (Pos e)) => Router e tok () a -> tok -> Either [e] [(a, tok)]
parse p s = 
    case partitionEithers $ runParser (prs p) s initialPos of
      ([], [])   -> Right []
      (errs, []) -> Left $ errs
      (_, fs)    -> Right [ (f (), tok) | ((f, tok),_) <- fs ]
-}

bestErrors :: (ErrorPosition e, Ord (Pos e)) => [e] -> [e]
bestErrors = maximumsBy (compare `on` getPosition)

--      (Left errs)     -> Left errs
--      (Right fs) -> Right [ (f (), tok) | (f, tok) <- fs ]
{-
-- | Give the first parse, for Routers with a parser that yields just one value.
parse1 :: (Error e, Position (Pos e)) => Router e tok () (a :- ()) -> tok -> Either [e] a
parse1 p s = 
    case parse p s of 
      (Left e) -> (Left e)
      (Right as) -> 
          case map (hhead . fst) as of
            [] -> Left [strMsg "no complete parses."]
            (a:_) -> Right a
-}
-- | Give all possible serializations.
unparse :: tok -> Router e tok () url -> url -> [tok]
unparse tok p = (map (($ tok) . fst)) . ser p


-- | Give the first serialization, for Routers with a serializer that needs just one value.
unparse1 :: (Error e) => tok -> Router e tok () (a :- ()) -> a -> Maybe tok
unparse1 tok p a = 
    case unparse tok p (a :- ()) of
      [] -> Nothing
      (s:_) -> Just s
{-
      (Left e)   -> Left e
      (Right []) -> Left (strMsg "unparse1: no valid unparsing")
      (Right (s:_)) -> Right s
-}

{-
data Router tok a b = Router
  { prs :: tok -> Either RouteError [(a -> b, tok)]
  , ser :: b -> Either RouteError [(tok -> tok, a)]
  }


routeError :: ErrorMsg -> Either RouteError b
routeError e = Left (RouteError Nothing e)
-}



{-
mapRouteError :: (e -> e') -> Router e a b -> Router e' a b
mapRouteError f (Router pf sf) =
    Router (\a -> either (Left . f) (Right . id) (pf a))
           (\b -> either (Left . f) (Right . id) (sf b))

instance Category (Router tok) where
  id = Router
    (\x -> return [(id, x)])
    (\x -> return [(id, x)])

  ~(Router pf sf) . ~(Router pg sg) = Router 
    (compose (.) pf pg)
    (compose (.) sf sg) 
-} 
{-
    Parser $ \tok ->
        do (f, tok')  <- runParser mf tok
           (g, tok'') <- runParser mg tok'
           return (f `op` g, tok'')
-}


{-
instance Applicative (Parser e tok) where
    pure =         
        Parser $ \tok pos ->
            [Right ((a, tok), pos)]
    (Parser f) >>= (Parser x) =
        Parser $ \tok pos ->
            concat [ (runParser (f a)) tok' pos' | Right ((a, tok'), pos') <- f tok pos ]
-}

    

                         
        
    

-- type P pos e tok a = tok -> pos -> [Either e ((a, tok), pos)]



{-
instance Monad (Parser e tok) where
    return a = Parser $ \tok -> return (a, tok)
    (Parser p) >>= f =
        Parser $ \tok ->
            do (a, tok') <- p tok
               (b, tok'') <- runParser (f a) tok'
               return (b, tok'')

instance MonadPlus (Parser e tok) where
    mzero = Parser $ \tok -> ListT $ return []
    (Parser x) `mplus` (Parser y) =
        Parser $ \tok -> 
            ListT $ StateT $ \st ->
                case runStateT (runListT (x tok)) st of
                  (Left _) -> 
                      runStateT (runListT (y tok)) st
                  (Right (a, st')) ->
                      case runStateT (runListT (y tok)) st of
                        (Left _) -> Right (a, st')
                        (Right (b, st'')) -> Right (a ++ b, st'')

instance Applicative (Parser e tok) where
    pure    = return
    f <*> x = f `ap` x


composeP
  :: (a -> b -> c)
  -> Parser e tok a
  -> Parser e tok b
  -> Parser e tok c
composeP op mf mg = 
    Parser $ \tok ->
        do (f, tok')  <- runParser mf tok
           (g, tok'') <- runParser mg tok'
           return (f `op` g, tok'')

bind :: Either e [a] -> (a -> Either e [b]) -> Either e [b]
bind (Left e) _ = (Left e)
bind (Right l) f =
    case partitionEithers (map f l) of
      ([], []) -> Right []
      (errs,[]) -> Left (last errs)
      (_, succ) -> Right (concat succ)
composeE
  :: (a -> b -> c)
  -> (i -> Either e [(a, j)])
  -> (j -> Either e [(b, k)])
  -> (i -> Either e [(c, k)])
composeE op mf mg = \s ->
  case mf s of
    (Left e) -> (Left e)
    (Right fs) ->
        case partitionEithers [ fmap (map (first (op f))) (mg s') | (f, s') <- fs  ] of
          ([], []) -> Right []
          (errs,[]) -> Left (last errs)
          (_, succs) -> Right (concat succs)


-}