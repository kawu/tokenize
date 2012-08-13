module Text.Tokenize.SRX
( mkSRXC
, SRXC
, tokenize
) where

import System.IO.Unsafe (unsafeInterleaveIO)
import Control.Applicative ((<$>))
import Data.List (intercalate)
import Codec.Binary.UTF8.String (encodeString, decodeString)
import Text.Regex.PCRE.String

import qualified Data.SRX as X

data BinTree a
  = Node
    { left  :: BinTree a
    , value :: a
    , right :: BinTree a }
  | Leaf
    { value :: a }

data SRXC = SRXC
    { rules     :: Regex
    -- | Internal nodes include summed regex expressions from subtrees.
    -- All expressions match only an entire string.
    , breakTree :: BinTree Regex }

type Rule = X.Rule String

ruleFlat :: Rule -> String
ruleFlat rule = X.before rule ++ X.after rule

ruleDeep :: Rule -> (String, String)
ruleDeep rule = (X.before rule, X.after rule)

mkSRXC :: [Rule] -> IO SRXC
mkSRXC rs = do
    rules <- mkRegex . intercalate "|" . map ruleFlat $ rs
    tree  <- mkBreakTree . map ruleDeep . filter X.break $ rs
    return (SRXC rules tree)

mkRegex :: String -> IO Regex
mkRegex xs =
    compile compUTF8 0 (encodeString xs) >>= \x -> case x of
        Left (offset, msg) -> error $ "mkRegex: " ++ show (offset, msg)
        Right re -> return re

-- | Make a break tree from a list of regex strings.
mkBreakTree :: [(String, String)] -> IO (BinTree Regex)
mkBreakTree []     = error "mkBreakTree: null argument"
mkBreakTree [ptrn] = Leaf <$> mkRegex (mkPtrn (addParens  ptrn))
mkBreakTree  ptrs  = do
    let (xs, ys) = divide ptrs
    left  <- mkBreakTree xs
    right <- mkBreakTree ys
    re    <- mkRegex $ intercalate "|" $ map mkPtrn ptrs
    return (Node left re right)

divide :: [a] -> ([a], [a])
divide (x:y:rest) =
    let (xs, ys) = divide rest
    in  (x:xs, y:ys)
divide [x] = ([x], [])
divide [] = ([], [])

addParens :: (String, String) -> (String, String)
addParens (xs, ys) =
    let f xs = '(' : xs ++ ")"
    in (f xs, f ys)

mkPtrn :: (String, String) -> String
mkPtrn (xs, ys) = '^' : xs ++ ys ++ "$"

matchAll :: Regex -> String -> IO (Maybe (String, String, String, [String]))
matchAll re xs = do
    m <- regexec re xs
    case m of
        Left msg -> error $ "matchAll: " ++ show msg
        Right t  -> return t

isMatch :: Regex -> String -> IO Bool
isMatch re xs = matchAll re xs >>= \m -> return $ case m of
    Just _  -> True
    Nothing -> False

matchCxt :: Regex -> String -> IO (Maybe (String, String, String))
matchCxt re xs =
    let getIt (x, y, z, _) = (x, y, z)
    in  fmap (fmap getIt) (matchAll re xs)

matchGroups :: Regex -> String -> IO (Maybe [String])
matchGroups re xs =
    let getIt (_, _, _, x) = x
    in  fmap (fmap getIt) (matchAll re xs)

tryBreak :: SRXC -> String -> IO (Maybe (String, String))
tryBreak srx xs = do
    let root = breakTree srx
    matchAll (value root) xs >>= \x -> case x of
        Nothing -> return Nothing
        Just _  -> doIt root xs
  where
    doIt (Node left _ right) xs = isMatch (value left) xs >>= \x -> case x of
        True  -> doIt left xs
        False -> doIt right xs
    doIt (Leaf re) xs =
        let mkPair [x, y] = (x, y)
        in  fmap (fmap mkPair) $ matchGroups re xs

tokenize :: SRXC -> String -> IO [String]
tokenize srx xs = map decodeString <$> tokenizeUtf8 srx (encodeString  xs)

tokenizeUtf8 :: SRXC -> String -> IO [String]
tokenizeUtf8 srx xs = do
    matchCxt (rules srx) xs >>= \m -> case m of
        Nothing -> return [xs]
        Just (before, match, after) -> do
            part <- tryBreak srx match >>= \b -> return $ case b of
                Nothing       -> [before ++ match]
                Just (lp, rp) -> [before ++ lp, rp]
            -- | Unsafe IO to make tokenizeUtf8 function lazy.
            rest <- unsafeInterleaveIO $ tokenizeUtf8 srx after
            return (part <@> rest)
  where
    [ys]     <@> (zs:zss) = (     (ys ++ zs) : zss)
    [xs, ys] <@> (zs:zss) = (xs : (ys ++ zs) : zss)
