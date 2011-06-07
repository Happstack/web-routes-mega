{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeOperators #-}
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
import Data.List (intercalate, sort, nub, stripPrefix)
import Data.String (IsString(..))
import Text.Zwaluw.Core
import Text.Zwaluw.Pos
import Text.Zwaluw.Combinators
import Text.Zwaluw.HList
import Text.Zwaluw.TH
import Web.Routes (Site(..))

data RouteError = RouteError (Maybe XYPos) [ErrorMsg]
    deriving (Eq, Read, Show, Ord, Typeable, Data)

type instance Pos RouteError = XYPos

instance ErrorPosition RouteError where
    getPosition (RouteError mPos _) = mPos

data ErrorMsg
    = SysUnExpect String
    | UnExpect String
    | Expect String
    | Message String
    | RouteEOF
    | RouteEOS
      deriving (Eq, Ord, Read, Show, Typeable, Data)

messageString :: ErrorMsg -> String
messageString (Expect s)         = s
messageString (UnExpect s)       = s
messageString (SysUnExpect s)    = s
messageString RouteEOF           = "end of input"
messageString RouteEOS           = "end of segment"
messageString (Message s)        = s

{-
instance ErrorList RouteError where
    listMsg s = [RouteError Nothing (Other s)]
-}
instance Error RouteError where
    strMsg s = RouteError Nothing [Message s]

throwRouteError pos e = [Left (RouteError (Just pos) e)]


-- instance a ~ b => IsString (Router RouteError [String] a b) where
--  fromString = lit

-- | Routes a constant string.
lit :: String -> Router RouteError [String] r r
lit l = Router pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> throwRouteError pos [RouteEOF]
             ("":_) | (not $ null l) -> throwRouteError pos [RouteEOS]
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') -> 
                       do [Right ((id, p':ps), addY (length l) pos)]
                   Nothing -> 
                       throwRouteError pos [UnExpect (show p), Expect (show l)]
      sf b = [(( \(s:ss) -> ((l ++ s) : ss)), b)]


infix  0 <?>
(<?>) :: Router RouteError tok a b -> String -> Router RouteError tok a b
router <?> msg = 
    router { prs = Parser $ \tok pos ->
        map (either (\(RouteError mPos errs) -> Left $ RouteError mPos ((Expect msg) : errs)) Right) (runParser (prs router) tok pos) }


infixr 9 </>
(</>) :: Router RouteError [String] b c -> Router RouteError [String] a b -> Router RouteError [String] a c
f </> g = f . eops . g

eops :: Router RouteError [String] r r
eops = Router 
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), pos)]
                   ("":ps) -> 
                          [ Right ((id, ps), addY 1 pos) ]
                   (p:_) -> throwRouteError pos [Message $ "path-segment not entirely consumed: " ++ p])
       (\a -> [(("" :), a)])

satisfy :: (Char -> Bool) -> Router RouteError [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> throwRouteError pos [RouteEOF]
         ("":ss)     -> throwRouteError pos [RouteEOS]
         ((c:cs):ss)
             | p c -> 
                 do [Right ((c, cs : ss), addX 1 pos )]
             | otherwise -> 
                 do throwRouteError pos [SysUnExpect [c]]
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])


-- check that the remainder of the 'String' in this segment satisfies a prefix
satisfyStr :: (String -> Bool) -> Router RouteError [String] r (String :- r)
satisfyStr p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> throwRouteError pos [RouteEOF]
         ("":ss)     -> throwRouteError pos [RouteEOS]
         (s:ss)
             | p s -> 
                 do [Right ((s, "":ss), addY 1 pos )]
             | otherwise -> 
                 do throwRouteError pos [SysUnExpect s]
  )
  (\str -> [ \paths -> case paths of [] -> [str] ; (s:ss) -> ((str++s):ss) | p str ])


digit :: Router RouteError [String] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters, plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: Router RouteError [String] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

anyChar :: Router RouteError [String] r (Char :- r)
anyChar = satisfy (const True)

char :: Char -> Router RouteError [String] r (Char :- r)
char c = satisfy (== c) <?> show [c]

int :: Router RouteError [String] r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | Routes any string.
-- FIXME: not sure the ss function is really doing the right thing
anyString :: Router RouteError [String] r (String :- r)
anyString = val ps ss 
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> throwRouteError pos [RouteEOF]
             ("":_) -> throwRouteError pos [RouteEOS]
             (s:ss) -> [Right ((s, ss), addY 1 pos)]
      ss str = [\ss -> str : ss]

-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@. 
printAs :: Router e [String] a b -> String -> Router e [String] a b
printAs r s = r { ser = map (first (const (s :))) . take 1 . ser r }

toSite :: ((url -> [(String, String)] -> String) -> url -> a) 
       -> Router RouteError [String] () (url :- ()) 
       -> Site url a
toSite handler r@(Router pf sf) =
    Site { handleSite = handler
         , formatPathSegments =  \url ->
             case unparse1 [] r url of
               Nothing -> error "formatPathSegments failed to produce a url"
               (Just ps) -> (ps, [])
         , parsePathSegments = mapLeft show . (parse1 isComplete r)
         }
    where
      mapLeft f = either (Left . f) Right

isComplete :: [String] -> Bool
isComplete []   = True
isComplete [""] = True
isComplete _    = False

condenseErrors :: [RouteError] -> RouteError
condenseErrors errs = 
    case bestErrors errs of
      [] -> RouteError Nothing []
      errs'@(RouteError pos _ : _) ->
          RouteError pos (nub $ concatMap (\(RouteError _ e) -> e) errs')

showErrorMessages :: String -> String -> String -> String -> String -> [ErrorMsg] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = intercalate ("; ") $ clean $  [showSysUnExpect, showUnExpect, showExpect, showMessages] -- , showExpect, showMessages]
    where
      isSysUnExpect (SysUnExpect {}) = True
      isSysUnExpect _                = False

      isUnExpect (UnExpect {})       = True
      isUnExpect _                   = False

      isExpect (Expect {})           = True
      isExpect _                     = False

      (sysUnExpect,msgs1) = span isSysUnExpect (sort msgs)
      (unExpect   ,msgs2) = span isUnExpect msgs1
      (expect     ,msgs3) = span isExpect msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect 
          | null sysUnExpect = ""
          | otherwise        = msgUnExpected ++ " " ++ (messageString $ head sysUnExpect)
      showMessages      = showMany "" msgs3

      showMany pre msgs = case clean (map messageString msgs) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []         = ""
      commasOr [m]        = m
      commasOr ms         = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep            = seperate ", " . clean

      seperate   _ []     = ""
      seperate   _ [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean               = nub . filter (not . null)
      

-- instance Show RouteError where
showRouteError strs (RouteError mPos msgs) =
        let posStr = case mPos of
                       Nothing -> "unknown position"
                       (Just (XYPos s c)) -> "segment " ++ show s ++ ", character " ++ show c
        in "parse error at " ++ posStr ++ ": " ++ (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs) ++
           " while parsing segments " ++ show strs
        