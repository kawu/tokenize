module Data.SRX
( SRX (..)
, Lang (..)
, Rule (..)
) where

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
