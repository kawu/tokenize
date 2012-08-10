import Control.Applicative ((<$>))
import System.Environment (getArgs)

import Data.SRX (SRX(..))
import Text.SRX (parseSRX)
import Text.Regex.Parse (parseRegex)

main = do
    [srxPath] <- getArgs
    srx <- fmap parseRegex . parseSRX <$> readFile srxPath
    mapM_ print (unSRX srx)
