import Control.Applicative ((<$>))
import System.Environment (getArgs)
import Text.Regex.Applicative (findLongestInfix)

import Data.SRX (SRX(..), mergeBy)
import Text.SRX (parseSRX)
import Text.Regex.Parse (parseRegex)
import Data.Regex.Parse (mkToks, mkRE)

main = do
    [srxPath, inPath] <- getArgs
    
--     srx <- fmap parseRegex . parseSRX <$> readFile srxPath
--     mapM_ print (unSRX srx)

    srx <- fmap (mkRE . parseRegex) . parseSRX <$> readFile srxPath
    let re = mergeBy ["Polish"] srx
    xs <- mkToks <$> readFile inPath
    let (Just (r1, r2, r3)) = findLongestInfix re xs
    print r1
    print r2
    print r3

    -- mapM_ print xs
