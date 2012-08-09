module Text.Regex.Parse
( parseRegex
) where

import Data.Char (GeneralCategory)
import Text.ParserCombinators.Poly
import Data.Regex

type P a = Parser Char a

{-# INLINE regexP #-}
regexP :: P Regex
regexP = unionP

unionP :: P UnionRE
unionP = UnionRE <$> seqP `sepBy` char '|'

seqP :: P SeqRE
seqP = SeqRE <$> many1 pieceP

pieceP :: P ModRE
pieceP = ModRE <$> atomP <*> modP

atomP :: P AtomRE
atomP = Brackets <$> bracket (char '(') (char ')') regexP
    <|> WordBoundary <$ string "\\b"
    <|> Symbol <$>
        (   '\t' <$ string "\\t"
        <|> '\n' <$ string "\\n"
        <|> '\r' <$ string "\\r" )
    <|> Space <$ string "\\s"
    <|> UniProp True <$> (string "\\p" *> uniPropP)
    <|> UniProp False <$> (string "\\P" *> uniPropP)
    <|> Symbol <$> (string "\\u" *> hexP)
--     <|> rangeP
    <|> Symbol <$> foldl1 (<|>)
        [ do { char '\\'; char x }
        | x <- "*?+[(){}^$|\\./" ]
    <|> LineBeg <$ char '^'
    <|> LineEnd <$ char '$'
    <|> Dot <$ char '.'

uniPropP :: P GeneralCategory
uniPropP = undefined

hexP :: P Char
hexP = undefined

modP :: P Mod
modP  = Star <$ char '*'
    <|> Plus <$ char '+'

char :: Char -> P Char
char c = satisfy (==c)

string :: String -> P String
string "" = return ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

parseRegex :: String -> Regex
parseRegex xs = case runParser regexP xs of
    (Left msg, _)   -> error $ "[parseRegex] " ++ msg
    (Right x, [])   -> x
    (_, xs)         -> error $ "[parseRegex] tokens not consumed: " ++ xs

-- -- | Suffix.
-- *
-- +
-- ?
-- {n} 
-- {n,} 
-- {n,m} 
-- *? 
-- +? 
-- ?? 
-- {n}? 
-- {n,}? 
-- {n,m}? 
-- *+
-- ++
-- ?+
-- {n}+ 
-- {n,}+ 
-- {n,m}+
-- 
-- -- | Infix.
-- | 
-- "concatenation" -- nie odpowiada mu żaden operator
