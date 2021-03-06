-- |
-- Module      : Console.Options.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Console.Options.Types
    ( Argument(..)
    , Command(..)
    , CommandHier(..)
    , Action
    , ActionWrapper(..)
    , UnnamedIndex
    -- * User Binders to retrieve their options
    , Flag(..)
    , FlagLevel(..)
    , FlagParam(..)
    , FlagMany(..)
    , Arg(..)
    , ArgRemaining(..)
    , Params(..)
    , Param(..)
    ) where

import           Console.Options.Flags (FlagDesc)
import           Console.Options.Nid
import           Data.Maybe

-- | A unnamed argument
data Argument =
      Argument
        { argumentName        :: String
        , argumentDescription :: String
        , argumentValidate    :: String -> Maybe String
        }
    | ArgumentCatchAll
        { argumentName        :: String
        , argumentDescription :: String
        }

-- | Represent a boolean flag (present / not present)
data Flag a where
    Flag       :: Nid -> Flag Bool

-- | Represent a Flag that can be called multiples times and will increase a counter.
data FlagLevel a where
    FlagLevel  :: Nid -> FlagLevel Int

-- | Represent a Flag with an optional or required value associated
data FlagParam a where
    FlagParamOpt     :: Nid -> a -> (String -> a) -> FlagParam a
    FlagParam        :: Nid -> (String -> a) -> FlagParam a

-- | Represent a Flag with optional or required value that can be added multiple times
newtype FlagMany a = FlagMany (FlagParam a)

-- | A positional argument
data Arg a where
    Arg           :: UnnamedIndex -> (String -> a) -> Arg a

-- | All the remaining positional arguments
data ArgRemaining a where
    ArgsRemaining :: ArgRemaining [String]

-- | Positional argument index
type UnnamedIndex = Int

-- | A command that is composed of a hierarchy
data Command r = Command
    { getCommandHier        :: CommandHier r
    , getCommandDescription :: String
    , getCommandOptions     :: [FlagDesc]
    , getCommandAction      :: ActionWrapper r
    }

-- | Recursive command tree
data CommandHier r =
      CommandTree [(String, Command r)]
    | CommandLeaf [Argument]

-- | A dictionary of parsed flags and arguments
data Params = Params
    { paramsFlags         :: [(Nid, Maybe String)] -- ^ return all the flags and their unique identifier. internal only
    , paramsPinnedArgs    :: [String]
    , paramsRemainingArgs :: [String]
    }

-- | Represent a program to run
type Action r = (forall a p . Param p => p a -> Ret p a) -> r

-- | Wrapper for Action or no Action.
data ActionWrapper r = 
      ActionWrapped (Action r)
    | NoActionWrapped

-- | Transform a binded argument or flag into a haskell value
class Param p where
    -- | Return value data type associated with a specific Param shape.
    type Ret p a :: *
    -- | get the value associated with a specific Param (either a Flag, FlagParam, or an Arg)
    getParams :: Params -> (forall a . p a -> Ret p a)

{-
flag :: optional on command line
    | no value       -> Bool
param :: optional on command line but with a value
    | optional value -> Maybe a
    | required value -> Maybe a
arg :: required on command line
    | required value (itself) -> a
-}
instance Param Flag where
    type Ret Flag a = Bool
    getParams (Params flagArgs _ _) (Flag nid) =
        isJust $ lookup nid flagArgs
instance Param FlagLevel where
    type Ret FlagLevel a = Int
    getParams (Params flagArgs _ _) (FlagLevel nid) =
        length $ filter ((== nid) . fst) flagArgs
instance Param FlagParam where
    type Ret FlagParam a = Maybe a
    getParams (Params flagArgs _ _) (FlagParamOpt nid a p) =
        case lookup nid flagArgs of
            Just (Just param) -> Just (p param)
            Just Nothing      -> Just a
            Nothing           -> Nothing
    getParams (Params flagArgs _ _) (FlagParam nid p) =
        case lookup nid flagArgs of
            Just Nothing      -> error "internal error: parameter is missing" -- something is wrong with the flag parser
            Just (Just param) -> Just (p param)
            Nothing           -> Nothing
instance Param FlagMany where
    type Ret FlagMany a = [a]
    getParams (Params flagArgs _ _) (FlagMany (FlagParamOpt nid a p)) =
        let margs = map snd $ filter ((== nid) . fst) flagArgs
         in map (maybe a p) margs
    getParams (Params flagArgs _ _) (FlagMany (FlagParam nid p)) =
        let margs = map snd $ filter ((== nid) . fst) flagArgs
         in map (maybe (error "") p) margs
instance Param Arg where
    type Ret Arg a = a
    getParams (Params _ unnamedArgs _) (Arg index p) =
        p (unnamedArgs !! index)
instance Param ArgRemaining where
    type Ret ArgRemaining a = [String]
    getParams (Params _ _ otherArgs) _ = otherArgs
