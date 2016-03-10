module Main where

import Console.Options
import Data.Char (isDigit)
import Data.Monoid

data MyADT = A | B | C
    deriving (Show,Eq)

main = defaultMain $ do

    programName "my-simple-program"
    programDescription "an optional description of your program that can be found in the help"

    -- a simple boolean flag -v or --verbose
    showFlag <- flag (FlagShort 'v' <> FlagLong "verbose" <> FlagDescription "activate verbose mode")

    -- a flag '-n' or '--name' requiring a string as parameter
    nameFlag <- flagParam (FlagShort 'n' <> FlagLong "name" <> FlagDescription "name of something")
                          (FlagRequired $ \s -> Right s)

    -- a flag 'i' or '--int' requiring an integer as parameter
    valueFlag <- flagParam (FlagShort 'i' <> FlagLong "int" <> FlagDescription "an integer value")
                           (FlagRequired $ \s -> if all isDigit s then Right (read s :: Int) else Left "invalid integer")

    -- a flag with an optional parameter, defaulting to some value
    xyzFlag <- flagParam (FlagShort 'x' <> FlagLong "xyz" <> FlagDescription "some ADT option")
                         (FlagOptional B (\s -> if s == "A" then Right A else if s == "B" then Right B else if s == "C" then Right C else Left "invalid value"))

    
    action $ \toParam -> do
        putStrLn $ show $ toParam showFlag
        putStrLn $ show $ toParam nameFlag
        putStrLn $ show $ toParam valueFlag
        putStrLn $ show $ toParam xyzFlag
