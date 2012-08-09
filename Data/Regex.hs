module Data.Regex
( Regex
, UnionRE (..)
, SeqRE (..)
, ModRE (..)
, Mod (..)
, AtomRE (..)
, RangeRE (..)
) where

import Data.Char (GeneralCategory)

type Regex = UnionRE

newtype UnionRE = UnionRE { unUnion :: [SeqRE] }

newtype SeqRE = SeqRE { unSeq :: [ModRE] }

data ModRE = ModRE
    { atom  :: AtomRE
    , mod   :: Mod }

-- | TODO: Complete the list of modifiers.
data Mod
    = Star
    | Plus

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

data RangeRE = RangeRE
    { ranNeg    :: Bool
    , ranElems  :: [(Char, Char)] }
