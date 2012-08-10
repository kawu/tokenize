module Data.Regex
( Regex
, UnionRE (..)
, SeqRE (..)
, ModRE (..)
, Mod (..)
, ModType (..)
, Greediness (..)
, AtomRE (..)
, RangeRE (..)
, RanPartRE (..)
) where

import Data.Char (GeneralCategory)
import Text.Regex.Applicative (Greediness (..))

type Regex = UnionRE

newtype UnionRE = UnionRE { unUnion :: [SeqRE] }
    deriving (Show)

newtype SeqRE = SeqRE { unSeq :: [ModRE] }
    deriving (Show)

data ModRE = ModRE
    { atom  :: AtomRE
    , mod   :: Mod }
    deriving (Show)

-- | RE modifier. It can be parametrized with modifier greediness.
data Mod
    = NoMod
    | Mod
        { modType   :: ModType
        , modGreed  :: Greediness }
    deriving (Show)

data ModType
    = Many              -- *
    | Some              -- +
    | Maybe             -- ?
    | Times Int         -- {n}
    | AtLeast Int       -- {n,}
    | Between Int Int   -- {n,m}
    deriving (Show)

data AtomRE
    = Symbol Char
    | Brackets UnionRE
    | WordBoundary
    -- | LineFeed       <- as Symbol
    -- | CarriageRet    <- as Symbol
    | UniProp Bool GeneralCategory
    | Space
    -- | Tab            <- as Symbol
    -- | HexSymbol Int  <- as Symbol
    | WordChar Bool
    | Range RangeRE
    | LineBeg
    | LineEnd
    | Dot
    deriving (Show)

data RangeRE = RangeRE
    { ranNeg    :: Bool
    , ranElems  :: [RanPartRE] }
    deriving (Show)

data RanPartRE
    = RanSymbol Char
    | RanUniProp Bool GeneralCategory
    | RanRange Char Char
    | RanEmbed RangeRE
    deriving (Show)
