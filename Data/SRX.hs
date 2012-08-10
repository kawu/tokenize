{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}

module Data.SRX
( SRX (..)
, Lang (..)
, Rule (..)
, merge
, mergeBy
, ruleRE
) where

import Prelude hiding (break)
import Data.List (find)
import Data.Maybe (maybeToList)
import Text.Regex.Applicative
import Data.Regex.Parse

-- | Header is ignored for now.  We assume, that 'cascading' flag is on.
newtype SRX t = SRX
    { unSRX :: [Lang t] }
    deriving (Show, Functor)

data Lang t = Lang
    { langName  :: String
    , rules     :: [Rule t] }
    deriving (Show, Functor)

data Rule t = Rule
    { break     :: Bool
    , before    :: t
    , after     :: t }
    deriving (Show, Functor)

data Result a
    = Break a a -- ^ When break rule fired
    | NoBreak a   -- ^ When no-break rule fired

merge :: [Rule (RE t [a])] -> RE t (Result [a])
merge [] = error "merge: empty list of rules"
merge rs = foldl1 (<|>) (map ruleRE rs)

-- | Merge rules from chosen languages.
mergeBy :: [String] -> SRX (RE t [a]) -> RE t (Result [a])
mergeBy names srx = merge . concat $
    [ rules lang
    | name <- names
    , lang <- maybeToList (find ((name==) . langName) (unSRX srx)) ]

ruleRE :: Rule (RE t [a]) -> RE t (Result [a])
ruleRE Rule{..}
    | break  	= Break	  <$> before <*> after
    | otherwise = noBreak <$> before <*> after
  where
    noBreak x y = NoBreak (x ++ y)
