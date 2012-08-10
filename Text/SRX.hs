module Text.SRX
( parseSRX
) where

import Control.Applicative ((<$>), (<*>))
import qualified Text.XML.PolySoup as Soup
import Text.XML.PolySoup hiding (XmlParser, Parser)

import qualified Data.SRX as S

type Parser a = Soup.XmlParser String a

type SRX    = S.SRX String
type Lang   = S.Lang String
type Rule   = S.Rule String

srxP :: Parser SRX
srxP = tag "srx" `joinR` do
    cut (tag "header")
    bodyP

bodyP :: Parser SRX
bodyP = S.SRX <$> (tag "body" `joinR` (langsP <* mapRulesP))

langsP :: Parser [Lang]
langsP = tag "languagerules" `joinR` many1 langP

langP :: Parser Lang
langP = (tag "languagerule" *> getAttr "languagerulename") `join`
  \name -> S.Lang name <$> many1 ruleP

ruleP :: Parser Rule
ruleP = (tag "rule" *> getAttr "break") `join` \break ->
    (S.Rule (break == "yes") <$> beforeP <*> afterP)

beforeP :: Parser String
beforeP = tag "beforebreak" `joinR` do
    maybe "" id <$> optional text

afterP :: Parser String
afterP = tag "afterbreak" `joinR` do
    maybe "" id <$> optional text

mapRulesP :: Parser ()
mapRulesP = cut (tag "maprules")

parseSRX :: String -> SRX
parseSRX = parseXML srxP
