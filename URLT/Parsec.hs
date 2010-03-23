module URLT.Parsec where

import Control.Applicative.Error(Failing(Failure,Success))
import URLT.AsURL
import Text.Parsec (runParser)
import Text.Parsec.String (Parser)

fromURLP :: Parser url -> (String -> Failing url)
fromURLP p =
  \str ->
  case runParser p () str str of
    (Left e) -> Failure [show e]
    (Right u) -> Success u
