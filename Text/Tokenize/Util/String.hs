module Text.Tokenize.Util.String
( unHyphen
) where

unHyphen :: String -> String
unHyphen ('‑':'\n':xs) = unHyphen xs
unHyphen (x:xs) = x : unHyphen xs
unHyphen [] = []
