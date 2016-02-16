-- |
-- Module      : Console.Options.Utils
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- utiliity. don't expose to users.
module Console.Options.Utils
    ( hPutErrLn
    ) where

import           System.IO (hPutStrLn, stderr)

-- | print to stderr and newline.
--
-- TODO add implementation for windows
hPutErrLn :: String -> IO ()
hPutErrLn = hPutStrLn stderr

