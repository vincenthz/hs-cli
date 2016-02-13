{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types #-}
module Console.Options.Monad
    ( ProgramDesc(..)
    , ProgramMeta(..)
    , OptionDesc
    , gatherDesc
    , getNextID
    , getNextIndex
    ) where

import           Control.Applicative
import           Console.Options.Nid
import           Console.Options.Types
import           Console.Options.Utils
import           Control.Monad.State
import           Control.Monad.Identity
import           System.Exit

-- the current state of the program description
-- as the monad unfold ..
data ProgramDesc r = ProgramDesc
    { stMeta        :: ProgramMeta
    , stCT          :: Command r     -- the command with the return type of actions
    , stNextID      :: !NidGenerator -- next id for flag
    , stNextIndex   :: !UnnamedIndex -- next index for unnamed argument
    }

data ProgramMeta = ProgramMeta
    { programMetaName        :: Maybe String
    , programMetaDescription :: Maybe String
    , programMetaVersion     :: Maybe String
    , programMetaHelp        :: [String]
    }

programMetaDefault :: ProgramMeta
programMetaDefault = ProgramMeta Nothing Nothing Nothing ["-h", "--help"]

-- OptionDesc (return value of action) a
newtype OptionDesc r a = OptionDesc { runOptionDesc :: StateT (ProgramDesc r) Identity a }
    deriving (Functor,Applicative,Monad,MonadState (ProgramDesc r))

gatherDesc :: OptionDesc r a -> ProgramDesc r
gatherDesc dsl = runIdentity $ execStateT (runOptionDesc dsl) initialProgramDesc

initialProgramDesc :: ProgramDesc r
initialProgramDesc = ProgramDesc { stMeta        = programMetaDefault
                                 , stCT          = iniCommand
                                 , stNextID      = nidGenerator
                                 , stNextIndex   = 0
                                 }
  where
    iniCommand :: Command r
    iniCommand = Command (CommandLeaf []) "..." [] Nothing

-- | Return the next unique argument ID
getNextID :: OptionDesc r Nid
getNextID = do
    (nid, nidGen) <- nidNext . stNextID <$> get
    modify $ \st -> st { stNextID = nidGen }
    return nid

getNextIndex :: OptionDesc r UnnamedIndex
getNextIndex = do
    idx <- stNextIndex <$> get
    modify $ \st -> st { stNextIndex = idx + 1 }
    return idx
