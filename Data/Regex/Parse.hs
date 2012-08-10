{-# LANGUAGE RecordWildCards #-}

module Data.Regex.Parse
( IsRE (..)
, Tok (..)
, mkToks
) where

import qualified Data.Set as S
import qualified Text.Regex.Applicative as R
import Text.Regex.Applicative hiding (RE)
import qualified  Data.Char as C
import Data.Char (isSpace, isAlphaNum)
import Data.Regex

data Tok 
    = Tok Char
    | Sep
        { wordBoundaryS  :: Bool
        , lineBegS       :: Bool
        , lineEndS       :: Bool }

isChar :: Tok -> Bool
isChar (Tok _) = True
isChar _       = False

isTrueSep :: Tok -> Bool
isTrueSep Sep{..}
    =  wordBoundaryS
    || lineBegS
    || lineEndS

isWB :: Tok -> Bool
isWB Sep{..} = wordBoundaryS
isWB _       = False

isLineBeg :: Tok -> Bool
isLineBeg Sep{..} = lineBegS
isLineBeg _       = False

isLineEnd :: Tok -> Bool
isLineEnd Sep{..} = lineEndS
isLineEnd _       = False

unChar :: Tok -> Char
unChar (Tok x) = x
unChar _       = error "unChar: token is not a character"

-- | Regex is a data structure representing a regular expression.
-- RE is its version transformed to an applicative parser.
type RE = R.RE Tok String

class IsRE t where
    mkRE :: t -> RE

instance IsRE UnionRE where
    mkRE (UnionRE xs) = foldl1 (<|>) (map mkRE xs)

instance IsRE SeqRE where
    mkRE (SeqRE xs) = foldl1 (<++>) (map mkRE xs)

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
f <++> g = (++) <$> f <*> g

instance IsRE ModRE where
    mkRE ModRE{..} = withMod mod (mkRE atom)

-- | Notation convention: all combinators ending with ' character
-- are non-greedy, while the rest is greedy.
withMod :: Mod -> RE -> RE
withMod NoMod = id
withMod Mod{..} = case (modType, modGreed) of
    (Many, Greedy)              -> concatR . many
    (Many, NonGreedy)           -> concatR . many'
    (Some, Greedy)              -> concatR . some
    (Some, NonGreedy)           -> concatR . some'
    (Maybe, Greedy)             -> maybeR  . optional
    (Maybe, NonGreedy)          -> maybeR  . optional'
    (Times n, _)                -> concatR . exactly n
    (AtLeast n, Greedy)         -> concatR . atLeast n
    (AtLeast n, NonGreedy)      -> concatR . atLeast' n
    (Between n m, Greedy)       -> concatR . between n m
    (Between n m, NonGreedy)    -> concatR . between' n m
  where
    concatR re = concat <$> re
    maybeR  re = maybe "" id <$> re

-- | Non-greedy version of many.
many' :: R.RE s a -> R.RE s [a]
many' = few

-- | Version of some which prefers less elements.
some' :: R.RE s a -> R.RE s [a]
some' re = (:) <$> re <*> few re

-- | Version of optional which prefers zero.
optional' :: R.RE s a -> R.RE s (Maybe a)
optional' re = pure Nothing <|> Just <$> re

-- | Parse re exactly n times.
exactly :: Int -> R.RE s a -> R.RE s [a]
exactly 0 re = pure []
exactly n re = (:) <$> re <*> exactly (n-1) re

atLeast :: Int -> R.RE s a -> R.RE s [a]
atLeast n re = (++) <$> exactly n re <*> many re

atLeast' :: Int -> R.RE s a -> R.RE s [a]
atLeast' n re = (++) <$> exactly n re <*> many' re

atMost :: Int -> R.RE s a -> R.RE s [a]
atMost 0 re = pure []
atMost n re = ((:) <$> re <*> atMost (n-1) re) <|> pure []

atMost' :: Int -> R.RE s a -> R.RE s [a]
atMost' 0 re = pure []
atMost' n re = pure [] <|> ((:) <$> re <*> atMost (n-1) re)

between :: Int -> Int -> R.RE s a -> R.RE s [a]
between n m re 
    | m >= n = (++) <$> exactly n re <*> atMost (m-n) re
    | otherwise = error "between: m < n"

between' :: Int -> Int -> R.RE s a -> R.RE s [a]
between' n m re 
    | m >= n = (++) <$> exactly n re <*> atMost' (m-n) re
    | otherwise = error "between': m < n"

instance IsRE AtomRE where
    mkRE atom = case atom of
        Symbol x            -> char x
        Brackets x          -> mkRE x
        WordBoundary        -> wb
        UniProp True cats   -> prop cats
        UniProp False cats  -> noProp cats
        Space               -> space
        WordChar True       -> wordChar
        WordChar False      -> notWordChar
        Range ranRE         -> mkRE ranRE
        LineBeg             -> lineBeg
        LineEnd             -> lineEnd
        Dot                 -> dot

instance IsRE RangeRE where
    mkRE RangeRE{..} =
        case ranNeg of
            False -> pchar p
            True  -> pchar (not.p)
      where
        ps  = map mkPred ranElems
        p x = or (map ($x) ps)

class IsPred t where
    mkPred :: t -> Char -> Bool

instance IsPred RangeRE where
    mkPred RangeRE{..} =
        m . p
      where
        m = case ranNeg of
            False -> id
            True  -> not
        ps  = map mkPred ranElems
        p x = or (map ($x) ps)

instance IsPred RanPartRE where
    mkPred (RanSymbol x)            = (==x)
    mkPred (RanUniProp True cats)   = hasProp cats
    mkPred (RanUniProp False cats)  = not . hasProp cats
    mkPred (RanRange p q)           = (&&) <$> (>=p) <*> (<=q)
    mkPred (RanEmbed ran)           = mkPred ran

-- | Ignore non-character tokens and check if the first character
-- satisfies the given predicate.
pchar :: (Char -> Bool) -> RE
pchar p = (:[]) . unChar <$>
    (  many (psym (not . isChar))
    *> psym (p . unChar) )

-- | Ignore non-character tokens and check if the first character
-- is equall to x.
char :: Char -> RE
char x = pchar (x==)

dot :: RE
dot = pchar (const True)

wb :: RE
wb = [] <$ psym isWB

lineBeg :: RE
lineBeg = [] <$ psym isLineBeg

lineEnd :: RE
lineEnd = [] <$ psym isLineEnd

hasProp :: [C.GeneralCategory] -> Char -> Bool
hasProp cats =
    let s = S.fromList cats
    in  flip S.member s . C.generalCategory

prop :: [C.GeneralCategory] -> RE
prop cat = pchar (hasProp cat)

noProp :: [C.GeneralCategory] -> RE
noProp cat = pchar (not . hasProp cat)

space :: RE
space = pchar isSpace

isWordChar :: Char -> Bool
isWordChar x = isAlphaNum x || C.generalCategory x == C.ConnectorPunctuation

wordChar :: RE
wordChar = pchar isWordChar

notWordChar :: RE
notWordChar = pchar (not . isWordChar)

-- | Make stream of tokens from a plain string.
-- TODO: handle "\n\r" and "\r\n" newlines.
mkToks :: String -> [Tok]
mkToks =
    filter (\x -> isChar x || isTrueSep x) . doIt
  where
    doIt (x:y:xs) =
        Tok x : Sep wb lb le : doIt (y:xs)
      where
        wb = isWordChar x `xor` isWordChar y
        lb = x == '\n'
        le = y == '\n'
    doIt [x] = [Tok x]
    doIt [] = []
    xor x y = not (x == y)
