module Data.Token
( Token (..)
, unToken
, unOrth
, unInterp
, unSpace
, unTokenize
) where

import Data.Monoid (Monoid, mconcat)

data Token t
    = Orth t
    | Interp t
    | Space t
    deriving (Show, Read, Eq, Ord)

unToken :: Token t -> t
unToken (Orth x) = x
unToken (Interp x) = x
unToken (Space x) = x

unOrth :: Token t -> Maybe t
unOrth (Orth x) = Just x
unOrth _        = Nothing

unInterp :: Token t -> Maybe t
unInterp (Interp x) = Just x
unInterp _          = Nothing

unSpace :: Token t -> Maybe t
unSpace (Space x) = Just x
unSpace _         = Nothing

-- | We can have many tokenization methods, but there is only
-- one sensible inverse operation, so we implement it here.
unTokenize :: Monoid t => [Token t] -> t
unTokenize = mconcat . map unToken
