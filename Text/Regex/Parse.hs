module Text.Regex.Parse
( parseRegex
) where

import qualified Data.Char as C
import qualified Data.Set as S
import Data.Char (isDigit, isAlphaNum)
import Data.Maybe (isJust)
import Numeric (readHex)
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
atomP = Brackets <$> bracketP '(' ')' regexP
    <|> WordBoundary <$ string "\\b"
    <|> Space <$ string "\\s"
    <|> UniProp True  <$> (string "\\p" *> uniPropP)
    <|> UniProp False <$> (string "\\P" *> uniPropP)
    <|> UniProp True  [C.DecimalNumber] <$ string "\\d"
    <|> Range  <$> rangeP
    <|> LineBeg <$ char '^'
    <|> LineEnd <$ char '$'
    <|> Dot <$ char '.'
    <|> Symbol <$> specialP
    <|> Symbol <$> satisfy (not . flip S.member escSet)
  where
    escSet = S.fromList escaped

specialP :: P Char
specialP =  '\t' <$ string "\\t"
    <|> '\n' <$ string "\\n"
    <|> '\r' <$ string "\\r"
    <|> string "\\u" *> hexP
    <|> escapedP

escapedP :: P Char
escapedP = foldl1 (<|>)
    [ do { char '\\'; char x }
    | x <- escaped ]

escaped :: String
escaped = "*?+[(){}^$|\\./"

hexP :: P Char
hexP  = toEnum . fst . head . readHex
    <$> exactly 4 alphaNumP

modP :: P Mod
modP  = Mod <$> modTypeP <*> modGreedP
    <|> pure NoMod

modTypeP :: P ModType
modTypeP
      = Many    <$ char '*'
    <|> Some    <$ char '+'
    <|> Maybe   <$ char '?'
    <|> do
            (p, c, q) <- bracketP '{' '}' $ (,,)
                <$> natP
                <*> optional (char ',')
                <*> optional natP
            return $ case (c, q) of
                (Nothing, _) -> Times p
                (_, Nothing) -> AtLeast p
                (_, Just q') -> Between p q'

modGreedP :: P Greediness
modGreedP = NonGreedy <$ char '?'
        <|> pure Greedy

natP :: P Int
natP = foldl1 op <$> some ( do
    x <- digit
    return (fromEnum x - fromEnum '0') )
  where
    m `op` n = 10*m + n
    digit = satisfy isDigit

rangeP :: P RangeRE
rangeP = bracketP '[' ']' $ do
    ranNeg <- isJust <$> optional (char '^')
    RangeRE ranNeg <$> some ranPartP

-- | TODO: Handle \b case.
ranPartP :: P RanPartRE
ranPartP
    =   RanUniProp True  <$> (string "\\p" *> uniPropP)
    <|> RanUniProp False <$> (string "\\P" *> uniPropP)
    <|> RanEmbed <$> rangeP
    <|> uncurry RanRange <$> do
            p <- alphaNumP
            char '-'
            q <- alphaNumP
            return (p, q)
    <|> RanSymbol <$> specialP
    <|> RanSymbol <$> satisfy (/=']')

char :: Char -> P Char
char c = satisfy (==c)

string :: String -> P String
string "" = return ""
string (x:xs) = do
    char x
    string xs
    return (x:xs)

bracketP :: Char -> Char -> P a -> P a
bracketP beg end p = bracket (char beg) (char end) p

uniPropP :: P [C.GeneralCategory]
uniPropP = bracketP '{' '}' (some alphaNumP) >>= \xs ->
  return $ case xs of
    "Lu" -> [C.UppercaseLetter]
    "Ll" -> [C.LowercaseLetter]
    "Lt" -> [C.TitlecaseLetter]
    "Lm" -> [C.ModifierLetter]
    "Lo" -> [C.OtherLetter]
    "L"  -> letterCats
    "Mn" -> [C.NonSpacingMark]
    "Mc" -> [C.SpacingCombiningMark]
    "Me" -> [C.EnclosingMark]
    "Nd" -> [C.DecimalNumber]
    "Nl" -> [C.LetterNumber]
    "No" -> [C.OtherNumber]
    "N"  -> numberCats
    "Pc" -> [C.ConnectorPunctuation]
    "Pd" -> [C.DashPunctuation]
    "Ps" -> [C.OpenPunctuation]
    "Pe" -> [C.ClosePunctuation]
    "Pi" -> [C.InitialQuote ]
    "Pf" -> [C.FinalQuote]
    "P"  -> punctCats
    "Po" -> [C.OtherPunctuation]
    "Sm" -> [C.MathSymbol]
    "Sc" -> [C.CurrencySymbol]
    "Sk" -> [C.ModifierSymbol]
    "So" -> [C.OtherSymbol]
    "Zs" -> [C.Space]
    "Zl" -> [C.LineSeparator]
    "Zp" -> [C.ParagraphSeparator]
    "Cc" -> [C.Control]
    "Cf" -> [C.Format]
    "Cs" -> [C.Surrogate]
    "Co" -> [C.PrivateUse]
    "Cn" -> [C.NotAssigned]
  where
    letterCats =
        [ C.UppercaseLetter
        , C.LowercaseLetter
        , C.TitlecaseLetter
        , C.ModifierLetter
        , C.OtherLetter ]
    punctCats = 
        [ C.ConnectorPunctuation
        , C.DashPunctuation
        , C.OpenPunctuation
        , C.ClosePunctuation
        , C.InitialQuote
        , C.FinalQuote ]
    numberCats = 
        [ C.DecimalNumber
        , C.LetterNumber
        , C.OtherNumber ]

alphaNumP :: P Char
alphaNumP = satisfy isAlphaNum

parseRegex :: String -> Regex
parseRegex xs = case runParser regexP xs of
    (Left msg, _)   -> error $ "[parseRegex] " ++ msg
    (Right x, [])   -> x
    (_, ys)         -> error $
        "[parseRegex] parsing: " ++ xs ++ "\n" ++
        "[parseRegex] tokens not consumed: " ++ ys
