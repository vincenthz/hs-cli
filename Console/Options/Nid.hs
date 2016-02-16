-- |
-- Module      : Console.Options.Nid
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- Somewhat similar in functionality to Data.Unique.
-- create Nids for parameter identification purpose.
module Console.Options.Nid
    ( Nid
    , NidGenerator
    , nidGenerator
    , nidNext
    ) where

-- | A unique ID
newtype Nid = Nid Int
    deriving (Show,Eq,Ord)

-- | A unique ID generator
newtype NidGenerator = NidGenerator Int

-- | Create a new unique generator
nidGenerator :: NidGenerator
nidGenerator = NidGenerator 0

-- | get the next unique id and return a new generator
nidNext :: NidGenerator -> (Nid, NidGenerator)
nidNext (NidGenerator next) = (Nid next, NidGenerator (next+1))
