module Data.SRX
( SRX (..)
, Lang (..)
, Rule (..)
-- , merge
-- , mkRE
) where

import Prelude hiding (break)
import Text.Regex.Applicative
import Text.Regex.Parse

-- | Header is ignored for now.  We assume, that 'cascading' flag is on.
type SRX = [Lang] 

data Lang = Lang
    { langName  :: String
    , rules     :: [Rule] }
    deriving (Show)

data Rule = Rule
    { break     :: Bool
    , before    :: String
    , after     :: String }
    deriving (Show)

-- -- | Now we would like to be able to fold (<|>) operation
-- -- on a list of rules:
-- 
-- data Result t
--     = Break [t] [t] -- ^ When break rule fired
--     | NoBreak [t]   -- ^ When no-break rule fired
-- 
-- merge :: [Rule] -> RE Char (Result Char)
-- merge [] = error "merge: empty list of rules"
-- merge rs = foldl1 (<|>) (map mkRE rs)
-- 
-- mkRE :: Rule -> RE Char (Result Char)
-- mkRE r
--     | break r   = Break <$> befR <*> aftR
--     | otherwise = noBreak <$> befR <*> aftR
--   where
--     befR = parseRegex (before r)
--     aftR = parseRegex (after r)
--     noBreak x y = NoBreak (x ++ y)
