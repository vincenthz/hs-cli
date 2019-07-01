{-# LANGUAGE FlexibleContexts #-}
module Main (main) where

import           Control.Applicative
import           Control.Monad

import           Test.Tasty
import           Test.Tasty.QuickCheck hiding (Success)
import           Test.QuickCheck.Monadic

import           System.Directory
import           Data.List
import           Data.Char (isDigit)
import           Data.Monoid
import           Data.Functor.Identity

import           Console.Options hiding (defaultMain)

import           Prelude hiding (exp)
import Debug.Trace
flagA = flagParam (FlagShort 'a' <> FlagLong "aaa") (FlagRequired Right)
flagB = flagParam (FlagShort 'b' <> FlagLong "bbb") (FlagOptional "xyz" Right)

commandFoo = command "foo" $ do
    action $ \_ -> return True

--commandBar :: Monad m => OptionDesc (m Bool) ()
commandBar :: OptionDesc (Identity Bool) ()
commandBar = command "bar" $ do
    _ <- flagA
    _ <- argument "arg1" Right
    action $ \_ -> do
        return True

testParseHelp name f = testProperty name $ runIdentity $ do
    case snd f of
        OptionHelp -> return True
        _          -> return False

testParseSuccess name values f =
    testProperty name $
        let (_,r) = f
         in case r of
                OptionSuccess p _   -> runIdentity $ return (sort values == sort (paramsFlags p))
                OptionHelp          -> error $ "got help expected success"
                OptionError s       -> error $ "got error " ++ show s ++ " expected success"
                OptionInvalid s     -> error $ "got invalid " ++ show s ++ " expected success"

testParseSuccessRun name f args validate =
    testProperty name $
        let (_,r) = parseOptions (f validate) args
         in case r of
                OptionSuccess p act -> runIdentity (act $ getParams p)
                OptionHelp          -> error $ "got help expected success"
                OptionError s       -> error $ "got error " ++ show s ++ " expected success"
                OptionInvalid s     -> error $ "got invalid " ++ show s ++ " expected success"

data PropertyResult = Success | Help | Error | Invalid
    deriving (Show,Eq)

isMatch (OptionSuccess {}) Success = True
isMatch (OptionHelp)       Help    = True
isMatch (OptionError {})   Error   = True
isMatch (OptionInvalid {}) Invalid = True
isMatch _                  _       = False

testMatch name f exp args = testProperty name $
    let (_,r) = parseOptions f args
     in isMatch r exp

flagParamIntParser :: String -> Either String Int
flagParamIntParser s
    | null s        = Left "integer expected"
    | all isDigit s = Right (read s :: Int)
    | otherwise     = Left "integer expected"

full :: OptionDesc (Identity ()) ()
full = do
    _ <- flagA
    _ <- flagB
    _ <- flagParam (FlagShort 'c' <> FlagLong "ccc") (FlagRequired flagParamIntParser)
    _ <- argument "arg1" Right
    action $ \_ -> do
        return ()

testMany :: ([Int] -> Bool) -> OptionDesc (Identity Bool) ()
testMany f = do
    m <- flagMany $ flagParam (FlagLong "opt") (FlagRequired flagParamIntParser)
    action $ \toParam -> do
        return $ f $ toParam m

testDefault :: (Maybe String -> Bool) -> OptionDesc (Identity Bool) ()
testDefault f = do
    flag <- flagB
    action $ \toParam -> do
      return $ f $ (traceShowId $ toParam flag)

main = defaultMain $ testGroup "options"
    [ testGroup "help"
        [ testParseHelp "1" $ parseOptions (commandBar) ["options", "argument", "--help", "a"]
        , testParseHelp "2" $ parseOptions (commandBar) ["options", "argument", "-h", "a"]
        ]
    , testGroup "success"
        [ testParseSuccess "1" [] $ parseOptions (commandBar >> commandFoo) ["bar", "arg1"]
        , testParseSuccess "2" [] $ parseOptions (commandBar >> commandFoo) ["foo", "a"]
        ]
    , testGroup "ex"
        [ testMatch "0" full Error   [] -- missing arg1
        , testMatch "1" full Success ["arg1-value"]
        , testMatch "2" full Success ["arg1-value", "-a", "val"]
        , testMatch "3" full Error   ["arg1-value", "-a"] -- missing value for -a
        , testMatch "4" full Error   ["arg1-value", "--aaa"] -- missing value for -a
        , testMatch "5" full Success ["arg1-value", "--aaa=val"]
        , testMatch "6" full Error   ["--aaa=val"] -- missing arg1
        , testMatch "7" full Success ["--bbb", "arg1-value"]
        , testMatch "8" full Success ["--ccc=1", "arg1-value"]
        , testMatch "9" full Error   ["--ccc=abc", "arg1-value"] -- error ccc flag expecting integer
        ]
    , testGroup "many"
        [ testParseSuccessRun "1" testMany [] (\l -> null l)
        , testParseSuccessRun "2" testMany ["--opt=1", "--opt", "2", "--opt=4", "--opt=3"] (\l -> sort l == [1,2,3,4])
        ]
    , testGroup "defaultParam"
        [ testParseSuccessRun "1" testDefault [] (\l -> l == Just "xyz") -- Fail: actually Nothing
        , testParseSuccessRun "2" testDefault ["-b", "value"] (\l -> l == Just "value") -- Fail: actually "xyz"
        ]
    ]
