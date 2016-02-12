module Console.Options.Utils
    ( hPutErrLn
    ) where

import           System.IO (hPutStrLn, stderr)

-- add implementation for windows
hPutErrLn :: String -> IO ()
hPutErrLn = hPutStrLn stderr

