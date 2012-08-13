import Control.Applicative ((<$>))
import System.Environment (getArgs)

import Data.SRX (SRX(..), rulesBy)
import Text.SRX (parseSRX)
import Text.Tokenize.SRX

main = do
    [srxPath, inPath] <- getArgs

    srx <- parseSRX <$> readFile srxPath
    srxc <- mkSRXC (rulesBy ["Polish"] srx)
    parts <- tokenize srxc =<< readFile inPath
    mapM_ putStrLn parts
