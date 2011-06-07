{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Text.Zwaluw.Pos where

import Data.Data

type family Pos a :: *

class ErrorPosition e where
    getPosition :: e -> Maybe (Pos e)

class Position a where
    initialPos :: a

data MajorMinorPos = MajorMinorPos 
    { major :: Integer 
    , minor :: Integer
    }
    deriving (Eq, Ord, Read, Show, Typeable, Data)

-- should this really be tied to the error type that uses it ?
-- if so, that affects addY
instance Position MajorMinorPos where
    initialPos = MajorMinorPos 0 0

addMajor :: (Integral i) => i -> MajorMinorPos -> MajorMinorPos
addMajor i (MajorMinorPos maj min) = MajorMinorPos (maj + (fromIntegral i)) 0

addMinor :: (Integral i) => i -> MajorMinorPos -> MajorMinorPos
addMinor i (MajorMinorPos maj min) = MajorMinorPos maj (min + (fromIntegral i))


