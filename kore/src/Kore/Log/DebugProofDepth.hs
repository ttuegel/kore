{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Log.DebugProofDepth
    ( DebugProofDepth (..)
    , debugProofDepth
    ) where

import Prelude.Kore

import Kore.Log.InfoProofDepth
    ( ProofDepth (..)
    )
import Log
import Pretty
    ( Pretty
    )
import qualified Pretty

newtype DebugProofDepth = DebugProofDepth ProofDepth
    deriving Show

instance Pretty DebugProofDepth where
    pretty (DebugProofDepth depth) = Pretty.pretty depth

instance Entry DebugProofDepth where
    entrySeverity _ = Debug
    shortDoc = Just . Pretty.pretty
    helpDoc _ = "log current proof depth"

debugProofDepth :: MonadLog log => ProofDepth -> log a -> log a
debugProofDepth depth = logWhile (DebugProofDepth depth)
{-# INLINE debugProofDepth #-}
