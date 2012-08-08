module Text.Tokenize.Simple.String
( tokenize
) where

import Control.Applicative ((<$>))
import Data.Char (isSpace, isPunctuation)
import Text.ParserCombinators.Poly.Lazy

import Data.Token

tokenize :: String -> [Token String]
tokenize =
    fst . runParser textP
  where
    textP = many segP
    segP = (Space   <$> spaceP)
       <|> (Interp  <$> interpP)
       <|> (Orth    <$> orthP)
    spaceP  = many1 $ satisfy isSpace
    interpP = many1 $ satisfy isPunctuation
    orthP   = many1 $ satisfy $ \c ->
        not (isPunctuation c) && not (isSpace c)
