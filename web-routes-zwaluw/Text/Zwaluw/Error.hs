{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Text.Zwaluw.Error where

import Control.Monad.Error (Error(..))
import Data.Data (Data, Typeable)
import Data.List (intercalate, sort, nub)
import Text.Zwaluw.Core
import Text.Zwaluw.Pos

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

infix  0 <?>
(<?>) :: Router RouteError tok a b -> String -> Router RouteError tok a b
router <?> msg = 
    router { prs = Parser $ \tok pos ->
        map (either (\(RouteError mPos errs) -> Left $ RouteError mPos ((Expect msg) : errs)) Right) (runParser (prs router) tok pos) }

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
showRouteError (RouteError mPos msgs) =
        let posStr = case mPos of
                       Nothing -> "unknown position"
                       (Just (XYPos c s)) -> "segment " ++ show s ++ ", character " ++ show c
        in "parse error at " ++ posStr ++ ": " ++ (showErrorMessages "or" "unknown parse error" "expecting" "unexpected" "end of input" msgs)
