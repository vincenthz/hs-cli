-- |
-- Module      : Console.Options.Monad
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Console.Options.Monad
    ( ProgramDesc(..)
    , ProgramMeta(..)
    , OptionDesc
    , gatherDesc
    , getNextID
    , getNextIndex
    , modify
    ) where

import           Control.Applicative
import           Console.Options.Nid
import           Console.Options.Types
import           Console.Options.Utils
import           System.Exit

import Foundation.Monad
import Foundation.Monad.State

-- | Ongoing State of the program description, as filled by monadic action
data ProgramDesc r = ProgramDesc
    { stMeta        :: ProgramMeta
    , stCT          :: Command r     -- the command with the return type of actions
    , stNextID      :: !NidGenerator -- next id for flag
    , stNextIndex   :: !UnnamedIndex -- next index for unnamed argument
    }

-- | Program meta information
data ProgramMeta = ProgramMeta
    { programMetaName        :: Maybe String -- ^ Program name (usually name of the executable)
    , programMetaDescription :: Maybe String -- ^ Program long description
    , programMetaVersion     :: Maybe String -- ^ Program version
    , programMetaHelp        :: [String]     -- ^ Flag that triggers Help.
    }

programMetaDefault :: ProgramMeta
programMetaDefault = ProgramMeta Nothing Nothing Nothing ["-h", "--help"]

-- | Option description Monad
newtype OptionDesc r a = OptionDesc { runOptionDesc :: StateT (ProgramDesc r) Identity a }
    deriving (Functor,Applicative,Monad)
instance MonadState (OptionDesc r) where
    type State (OptionDesc r) = ProgramDesc r
    withState f = OptionDesc $ withState f

-- | Run option description
gatherDesc :: OptionDesc r a -> ProgramDesc r
gatherDesc dsl = snd $ runIdentity $ runStateT (runOptionDesc dsl) initialProgramDesc

initialProgramDesc :: ProgramDesc r
initialProgramDesc = ProgramDesc { stMeta        = programMetaDefault
                                 , stCT          = iniCommand
                                 , stNextID      = nidGenerator
                                 , stNextIndex   = 0
                                 }
  where
    iniCommand :: Command r
    iniCommand = Command (CommandLeaf []) "..." [] NoActionWrapped

-- | Return the next unique argument ID
getNextID :: OptionDesc r Nid
getNextID = do
    (nid, nidGen) <- nidNext . stNextID <$> get
    withState $ \st -> ((), st { stNextID = nidGen })
    return nid

modify :: (ProgramDesc r -> ProgramDesc r) -> OptionDesc r ()
modify f = withState $ \st -> ((), f st)

-- | Return the next unique position argument ID
getNextIndex :: OptionDesc r UnnamedIndex
getNextIndex = do
    idx <- stNextIndex <$> get
    withState $ \st -> ((), st { stNextIndex = idx + 1 })
    return idx
