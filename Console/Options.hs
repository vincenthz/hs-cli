-- |
-- Module      : Console.Options
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Options parsing using a simple DSL approach.
--
-- Using this API, your program should have the following shape:
--
-- >defaultMain $ do
-- >    f1 <- flag ..
-- >    f2 <- argument ..
-- >    action $ \toParam ->
-- >        something (toParam f1) (toParam f2) ..
--
-- You can also define subcommand using:
--
-- >defaultMain $ do
-- >    subcommand "foo" $ do
-- >       <..flags & parameters definitions...>
-- >       action $ \toParam -> <..IO-action..>
-- >    subcommand "bar" $ do
-- >       <..flags & parameters definitions...>
-- >       action $ \toParam -> <..IO-action..>
--
-- Example:
--
-- >main = defaultMain $ do
-- >    programName "test-cli"
-- >    programDescription "test CLI program"
-- >    flagA    <- flag $ FlagShort 'a' <> FlagLong "aaa"
-- >    allArgs  <- remainingArguments "FILE"
-- >    action $ \toParam -> do
-- >        putStrLn $ "using flag A : " ++ show (toParam flagA)
-- >        putStrLn $ "args: " ++ show (toParam allArgs)
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Console.Options
    (
    -- * Running
      defaultMain
    , defaultMainWith
    , parseOptions
    , OptionRes(..)
    , OptionDesc
    -- * Description
    , programName
    , programVersion
    , programDescription
    , command
    , FlagFrag(..)
    , flag
    , flagParam
    , flagMany
    -- , conflict
    , argument
    , remainingArguments
    , action
    , description
    , Action
    -- * Arguments
    , ValueParser
    , FlagParser(..)
    , Flag
    , FlagLevel
    , FlagParam
    , FlagMany
    , Arg
    , ArgRemaining
    , Params
    , paramsFlags
    , getParams
    ) where

import Foundation (toList, toCount, fromList)

import           Console.Options.Flags hiding (Flag)
import qualified Console.Options.Flags as F
import           Console.Options.Nid
import           Console.Options.Utils
import           Console.Options.Monad
import           Console.Options.Types
import           Console.Display (justify, Justify(..))

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class

import           Data.List
import           Data.Maybe (fromMaybe)
import           Data.Version
import           Data.Functor.Identity

import           System.Environment (getArgs, getProgName)
import           System.Exit

----------------------------------------------------------------------
setDescription :: String -> Command r -> Command r
setDescription desc  (Command hier _ opts act)    = Command hier desc opts act

setAction :: Action r -> Command r -> Command r
setAction act (Command hier desc opts _)   = Command hier desc opts (ActionWrapped act)

addOption :: FlagDesc -> Command r -> Command r
addOption opt   (Command hier desc opts act) = Command hier desc (opt : opts) act

tweakOption :: Nid -> (FlagDesc -> FlagDesc) -> Command r -> Command r
tweakOption nid mapFlagDesc (Command hier desc opts act) =
    Command hier desc (modifyNid opts) act
  where
    modifyNid []           = []
    modifyNid (f:fs)
        | flagNid f == nid = mapFlagDesc f : fs
        | otherwise        = f : modifyNid fs

addArg :: Argument -> Command r -> Command r
addArg arg = modifyHier $ \hier ->
    case hier of
        CommandLeaf l  -> CommandLeaf (arg:l)
        CommandTree {} -> hier -- ignore argument in a hierarchy.
----------------------------------------------------------------------

-- | A parser for a flag's value, either optional or required.
data FlagParser a =
      FlagRequired (ValueParser a)   -- ^ flag value parser with a required parameter.
    | FlagOptional a (ValueParser a) -- ^ Optional flag value parser: Default value if not present to a

-- | A parser for a value. In case parsing failed Left should be returned.
type ValueParser a = String -> Either String a

-- | return value of the option parser. only needed when using 'parseOptions' directly
data OptionRes r =
      OptionSuccess Params (Action r)
    | OptionHelp
    | OptionError String -- user cmdline error in the arguments
    | OptionInvalid String -- API has been misused

-- | run parse options description on the action
--
-- to be able to specify the arguments manually (e.g. pre-handling),
-- you can use 'defaultMainWith'.
-- >defaultMain dsl = getArgs >>= defaultMainWith dsl
defaultMain :: OptionDesc (IO ()) () -> IO ()
defaultMain dsl = getArgs >>= defaultMainWith dsl

-- | same as 'defaultMain', but with the argument
defaultMainWith :: OptionDesc (IO ()) () -> [String] -> IO ()
defaultMainWith dsl args = do
    progrName <- getProgName
    let (programDesc, res) = parseOptions (programName progrName >> dsl) args
     in case res of
        OptionError s          -> putStrLn s >> exitFailure
        OptionHelp             -> help (stMeta programDesc) (stCT programDesc) >> exitSuccess
        OptionSuccess params r -> r (getParams params)
        OptionInvalid s        -> putStrLn s >> exitFailure

-- | This is only useful when you want to handle all the description parsing
-- manually and need to not automatically execute any action or help/error handling.
--
-- Used for testing the parser.
parseOptions :: OptionDesc r () -> [String] -> (ProgramDesc r, OptionRes r)
parseOptions dsl args =
    let descState = gatherDesc dsl
     in (descState, runOptions (stCT descState) args)

--helpSubcommand :: [String] -> IO ()

help :: ProgramMeta -> Command (IO ()) -> IO ()
help pmeta (Command hier _ commandOpts _) = do
    tell (fromMaybe "<program>" (programMetaName pmeta) ++ " version " ++ fromMaybe "<undefined>" (programMetaVersion pmeta) ++ "\n")
    tell "\n"
    maybe (return ()) (\d -> tell d >> tell "\n\n") (programMetaDescription pmeta)
    tell "Options:\n"
    tell "\n"
    mapM_ (tell . printOpt 0) commandOpts
    case hier of
        CommandTree subs -> do
            tell "\n"
            tell "Commands:\n"
            let cmdLength = maximum (map (length . fst) subs) + 2
            forM_ subs $ \(n, c) -> tell $ indent 2 (toList (justify JustifyRight (toCount cmdLength) (fromList n)) ++ getCommandDescription c ++ "\n")
            tell "\n"
            mapM_ (printSub 2) subs
        CommandLeaf _    ->
            return ()
  where
    tell = putStr
    printSub iLevel (name, cmdOpt) = do
        tell $ "\nCommand `" ++ name ++ "':\n\n"
        tell $ indent iLevel "Options:\n\n"
        mapM_ (tell . printOpt iLevel) (getCommandOptions cmdOpt)
        case getCommandHier cmdOpt of
            CommandTree subs -> do
                tell $ indent iLevel "Commands:\n"
                let cmdLength = maximum (map (length . fst) subs) + 2 + iLevel
                forM_ subs $ \(n, c) -> tell $ indent (iLevel + 2) (toList (justify JustifyRight (toCount cmdLength) (fromList n)) ++ getCommandDescription c ++ "\n")
                tell "\n"
                mapM_ (printSub (iLevel + 2)) subs
            CommandLeaf _ -> pure ()
        --tell . indent 2 ""

    printOpt iLevel fd =
        let optShort = maybe (replicate 2 ' ') (\c -> "-" ++ [c]) $ flagShort ff
            optLong  = maybe (replicate 8 ' ') (\s -> "--" ++ s) $ flagLong ff
            optDesc  = maybe "" ("  " ++) $ flagDescription ff
         in indent (iLevel + 2) $ intercalate " " [optShort, optLong, optDesc] ++ "\n"
      where
        ff = flagFragments fd

runOptions :: Command r -- commands
           -> [String] -- arguments
           -> OptionRes r
runOptions ct allArgs
    | "--help" `elem` allArgs = OptionHelp
    | "-h" `elem` allArgs     = OptionHelp
    | otherwise               = go [] ct allArgs
  where
        -- parse recursively using a Command structure
        go :: [[F.Flag]] -> Command r -> [String] -> OptionRes r
        go parsedOpts (Command hier _ commandOpts act) unparsedArgs =
            case parseFlags commandOpts unparsedArgs of
                (opts, unparsed, [])  -> do
                    case hier of
                        -- if we have sub commands, then we pass the unparsed options
                        -- to their parsers
                        CommandTree subs -> do
                            case unparsed of
                                []     -> errorExpectingMode subs
                                (x:xs) -> case lookup x subs of
                                                Nothing      -> errorInvalidMode x subs
                                                Just subTree -> go (opts:parsedOpts) subTree xs
                        -- no more subcommand (or none to start with)
                        CommandLeaf unnamedArgs ->
                            case validateUnnamedArgs (reverse unnamedArgs) unparsed of
                                Left err   -> errorUnnamedArgument err
                                Right (pinnedArgs, remainingArgs) -> do
                                    let flags = concat (opts:parsedOpts)
                                    case act of
                                        NoActionWrapped -> OptionInvalid "no action defined"
                                        ActionWrapped a  ->
                                            let params = Params flags
                                                                pinnedArgs
                                                                remainingArgs
                                             in OptionSuccess params a
                (_, _, ers) -> do
                    OptionError $ mconcat $ map showOptionError ers

        validateUnnamedArgs :: [Argument] -> [String] -> Either String ([String], [String])
        validateUnnamedArgs argOpts l =
            v [] argOpts >>= \(opts, _hasCatchall) -> do
                let unnamedRequired = length opts
                if length l < unnamedRequired
                    then Left "missing arguments"
                    else Right $ splitAt unnamedRequired l
          where
            v :: [Argument] -> [Argument] -> Either String ([Argument], Bool)
            v acc []                    = Right (reverse acc, False)
            v acc (a@(Argument {}):as)  = v (a:acc) as
            v acc ((ArgumentCatchAll {}):[]) = Right (reverse acc, True)
            v _   ((ArgumentCatchAll {}):_ ) = Left "arguments expected after remainingArguments"

        showOptionError (FlagError opt i s) = do
            let optName = (maybe "" (:[]) $ flagShort $ flagFragments opt) ++ " " ++ (maybe "" id $ flagLong $ flagFragments opt)
             in ("error: " ++ show i ++ " option " ++ optName ++ " : " ++ s ++ "\n")

        errorUnnamedArgument err =
            OptionError $ mconcat
                [ "error: " ++ err
                , ""
                ]

        errorExpectingMode subs =
            OptionError $ mconcat (
                [ "error: expecting one of the following mode:\n"
                , "\n"
                ] ++ map (indent 4 . (++ "\n") . fst) subs)
        errorInvalidMode got subs =
            OptionError $ mconcat (
                [ "error: invalid mode '" ++ got ++ "', expecting one of the following mode:\n"
                , ""
                ] ++ map (indent 4 . (++ "\n") . fst) subs)

indent :: Int -> String -> String
indent n s = replicate n ' ' ++ s

-- | Set the program name
--
-- default is the result of base's `getProgName`
programName :: String -> OptionDesc r ()
programName s = modify $ \st -> st { stMeta = (stMeta st) { programMetaName = Just s } }

-- | Set the program version
programVersion :: Version -> OptionDesc r ()
programVersion s = modify $ \st -> st { stMeta = (stMeta st) { programMetaVersion = Just $ showVersion s } }

-- | Set the program description
programDescription :: String -> OptionDesc r ()
programDescription s = modify $ \st -> st { stMeta = (stMeta st) { programMetaDescription = Just s } }

-- | Set the description for a command
description :: String -> OptionDesc r ()
description doc = modify $ \st -> st { stCT = setDescription doc (stCT st) }

modifyHier :: (CommandHier r -> CommandHier r) -> Command r -> Command r
modifyHier f (Command hier desc opts act) = Command (f hier) desc opts act

modifyCT :: (Command r -> Command r) -> OptionDesc r ()
modifyCT f = modify $ \st -> st { stCT = f (stCT st) }

-- | Create a new sub command
command :: String -> OptionDesc r () -> OptionDesc r ()
command name sub = do
    let subSt = gatherDesc sub
    modifyCT (addCommand (stCT subSt))
    --modify $ \st -> st { stCT = addCommand (stCT subSt) $ stCT st }
  where addCommand subTree = modifyHier $ \hier ->
            case hier of
                CommandLeaf _ -> CommandTree [(name,subTree)]
                CommandTree t -> CommandTree ((name, subTree) : t)

-- | Set the action to run in this command
action :: Action r -> OptionDesc r ()
action ioAct = modify $ \st -> st { stCT = setAction ioAct (stCT st) }

-- | Flag option either of the form -short or --long
--
-- for flag that doesn't have parameter, use 'flag'
flagParam :: FlagFrag -> FlagParser a -> OptionDesc r (FlagParam a)
flagParam frag fp = do
    nid <- getNextID

    let fragmentFlatten = flattenFragments frag

    let opt = FlagDesc
                { flagFragments   = fragmentFlatten
                , flagNid         = nid
                , F.flagArg       = argp
                , flagArgValidate = validator
                , flagArity       = 1
                }

    modify $ \st -> st { stCT = addOption opt (stCT st) }

    case mopt of
        Just a  -> return (FlagParamOpt nid a parser)
        Nothing -> return (FlagParam nid parser)
  where
    (argp, parser, mopt, validator) = case fp of
        FlagRequired p   -> (FlagArgHave, toArg p, Nothing, isValid p)
        FlagOptional a p -> (FlagArgMaybe, toArg p, Just a, isValid p)

    toArg :: (String -> Either String a) -> String -> a
    toArg p = either (error "internal error toArg") id . p

    isValid f = either FlagArgInvalid (const FlagArgValid) . f

-- | Apply on a 'flagParam' to turn into a flag that can
-- be invoked multiples, creating a list of values
-- in the action.
flagMany :: OptionDesc r (FlagParam a) -> OptionDesc r (FlagMany a)
flagMany fp = do
    f <- fp
    let nid = case f of
                FlagParamOpt n _ _ -> n
                FlagParam n _      -> n
    modify $ \st -> st { stCT = tweakOption nid (\fd -> fd { flagArity = maxBound }) (stCT st) }
    return $ FlagMany f

-- | Flag option either of the form -short or --long
--
-- for flag that expect a value (optional or mandatory), uses 'flagArg'
flag :: FlagFrag -> OptionDesc r (Flag Bool)
flag frag = do
    nid <- getNextID

    let fragmentFlatten = flattenFragments frag

    let opt = FlagDesc
                { flagFragments   = fragmentFlatten
                , flagNid         = nid
                , F.flagArg       = FlagArgNone
                , flagArgValidate = error ""
                , flagArity       = 0
                }

    modify $ \st -> st { stCT = addOption opt (stCT st) }
    return (Flag nid)

-- | An unnamed positional argument
--
-- For now, argument in a point of tree that contains sub trees will be ignored.
-- TODO: record a warning or add a strict mode (for developping the CLI) and error.
argument :: String -> ValueParser a -> OptionDesc r (Arg a)
argument name fp = do
    idx <- getNextIndex
    let a = Argument { argumentName        = name
                     , argumentDescription = ""
                     , argumentValidate    = either Just (const Nothing) . fp
                     }
    modifyCT $ addArg a
    return (Arg idx (either (error "internal error") id . fp))

-- | All the remaining position arguments
--
-- This is useful for example for a program that takes an unbounded list of files
-- as parameters.
remainingArguments :: String -> OptionDesc r (ArgRemaining [String])
remainingArguments name = do
    let a = ArgumentCatchAll { argumentName        = name
                             , argumentDescription = ""
                             }
    modifyCT $ addArg a
    return ArgsRemaining

-- | give the ability to set options that are conflicting with each other
-- if option a is given with option b then an conflicting error happens
-- conflict :: Flag a -> Flag b -> OptionDesc r ()
-- conflict = undefined
