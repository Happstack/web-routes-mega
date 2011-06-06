{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeOperators #-}
module Web.Routes.Zwaluw 
{-
    (
    -- * Types
    Router, RouteError(..), (:-)(..), (<>), (.~)
    
    -- * Running routers
  , parse, unparse
--  , parse1, unparse1
    
    -- * Router combinators
  , pure, xmap, xmaph
  , val, readshow, lit, push
  , opt, duck, satisfy, rFilter, printAs
  , manyr, somer, chainr, chainr1 
  , manyl, somel, chainl, chainl1
  
    -- * Built-in routers
  , int, string -- ,char, digit, hexDigit
  , (</>),
  {-
  , rNil, rCons, rList, rListSep
  , rPair
  , rLeft, rRight, rEither
  , rNothing, rJust, rMaybe
  , rTrue, rFalse
-}
    ) -}
    where

import Prelude hiding ((.), id, (/))
import Control.Arrow (first)
import Control.Monad (guard)
import Control.Monad.State
-- import Control.Monad.Trans.Error (Error(..))
import Control.Category
import Control.Monad.Error
-- import Control.Monad.Trans.Error (ErrorList(..))
import Data.Char
import Data.Data
import Data.Monoid
import Data.Char (isDigit, isHexDigit, intToDigit, digitToInt)
import Data.List (stripPrefix)
import Text.Zwaluw.Core
import Text.Zwaluw.Pos
import Text.Zwaluw.Combinators
import Text.Zwaluw.HList
import Text.Zwaluw.TH
import Web.Routes (Site(..))

data RouteError = RouteError (Maybe XYPos) ErrorMsg
    deriving (Eq, Ord, Read, Show, Typeable, Data)

type instance Pos RouteError = XYPos

instance ErrorPosition RouteError where
    getPosition (RouteError mPos _) = mPos

data ErrorMsg
    = InvalidLit String String
    | PredFail Char
    | RouteEOF
    | RouteEOS
    | Other String
      deriving (Eq, Ord, Read, Show, Typeable, Data)
{-
instance ErrorList RouteError where
    listMsg s = [RouteError Nothing (Other s)]
-}
instance Error RouteError where
    strMsg s = RouteError Nothing (Other s)
{-
throwRouteError :: (MonadState XYPos m, MonadError RouteError m) => ErrorMsg -> m b
throwRouteError e =
    do pos <- get
       throwError (RouteError (Just pos) e)
-}

throwRouteError pos e = [Left (RouteError (Just pos) e)]


{-
instance a ~ b => IsString (Router RouteError a b) where
  fromString = lit
-}

-- | Routes a constant string.
lit :: String -> Router RouteError [String] r r
lit l = Router pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> throwRouteError pos RouteEOF
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') -> 
                       do [Right ((id, p':ps), addX 1 pos)]
                   Nothing -> 
                       throwRouteError pos (InvalidLit p l)
      sf b = [(( \(s:ss) -> ((l ++ s) : ss)), b)]

infixr 9 </>
(</>) :: Router RouteError [String] b c -> Router RouteError [String] a b -> Router RouteError [String] a c
f </> g = f . eops . g

eops :: Router RouteError [String] r r
eops = Router 
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), pos)]
                   ("":ps) -> 
                          [ Right ((id, ps), addY 1 pos) ]
                   (p:_) -> throwRouteError pos (Other $ "path-segment not entirely consumed: " ++ p))
       (\a -> [(("" :), a)])

satisfy :: (Char -> Bool) -> Router RouteError [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> throwRouteError pos RouteEOF
         ("":ss)     -> throwRouteError pos RouteEOS
         ((c:cs):ss)
             | p c -> 
                 do [Right ((c, cs : ss), addX 1 pos )]
             | otherwise -> 
                 do throwRouteError pos (PredFail c)
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])

digit :: Router RouteError [String] r (Char :- r)
digit = satisfy isDigit

alpha :: Router RouteError [String] r (Char :- r)
alpha = satisfy isAlpha

char :: Char -> Router RouteError [String] r (Char :- r)
char c = satisfy (== c)

int :: Router RouteError [String] r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | Routes any string.
-- FIXME: not sure the ss function is really doing the right thing
string :: Router RouteError [String] r (String :- r)
string = val ps ss 
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> throwRouteError pos RouteEOF
             ("":_) -> throwRouteError pos RouteEOS
             (s:ss) -> [Right ((s, ss), addY 1 pos)]
      ss str = [\ss -> str : ss]

toSite :: ((url -> [(String, String)] -> String) -> url -> a) 
       -> Router RouteError [String] () (url :- ()) 
       -> Site url a
toSite handler r@(Router pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparse1 [] r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = \paths -> 
                               let results = parse r paths
                               in
                                 case [ a | (Right (a,[])) <- results ] of
                                   ((u :- ()):_) -> Right u
                                   _             -> Left $ show $ bestErrors [ e | Left e <- results ]
         }

{-
-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Router a b -> String -> Router a b
printAs r s = r { ser = map (first (const (s ++))) . take 1 . ser r }

readEither :: (Read a) => [String] -> StateT ErrorPos (Either RouteError) [(a, [String])]
readEither [] = throwRouteError RouteEOF
readEither (p:ps) = 
          case reads p of
            [] -> throwRouteError (Other $ "readEither failed on " ++ p)
            rs -> Right $ map (\(a,p') -> (a, p':ps)) rs




-- | Routes any value that has a Show and Read instance.
readshow :: (Show a, Read a) => Router [String] r (a :- r)
readshow = val readEither showEither


showEither :: (Show a) => a -> Either e [[String] -> [String]]
showEither a = Right [\(s:ss) -> (shows a s) : ss ]

-- | Routes any integer.
int :: Router [String] r (Int :- r)
int = readshow

-- | Routes any string.
string :: Router [String] r (String :- r)
string = val ps ss 
    where
      ps [] = routeError RouteEOF
      ps (h:t) = return [(h, ("" : t))]
      ss str = return [\(s:ss) -> (str ++ s) : ss]

-- | Routes one string satisfying the given predicate.
satisfy :: (String -> Bool) -> Router [String] r (String :- r)
satisfy p = val ps ss
    where
      ps []    = routeError RouteEOF
      ps (h:t) = if p h
                 then return [(h, t)]
                 else Left $ strMsg ("predicate failed on " ++ h)
      ss s = if p s
             then return [([s] ++)]
             else Left (strMsg $ "predicate failed on " ++ s)
infixr 9 </>
(</>) :: Router [String] b c -> Router [String] a b -> Router [String] a c
f </> g = f . eops . g

eops :: Router [String] r r
eops = Router 
       (\path -> case path of
                   []      -> return   [(id, [])]
                   ("":ps) -> return [(id, ps)]
                   (p:_) -> Left $ strMsg $ "path-segment not entirely consumed: " ++ p)
       (\a -> return [(("" :), a)])


-}
