{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA
-}

module Kore.Log.DebugProofState
    ( DebugProofState (..)
    ) where

import Prelude.Kore

import Kore.Rewriting.RewritingVariable
import Kore.Step.RulePattern
    ( ReachabilityRule (..)
    , RewriteRule (..)
    )
import Kore.Strategies.ProofState
    ( Prim (..)
    , ProofState (..)
    )
import Log
import Pretty
    ( Pretty (..)
    )
import qualified Pretty

data DebugProofState =
    DebugProofState
        { proofState :: ProofState ReachabilityRule
        , transition :: Prim (RewriteRule RewritingVariableName)
        , result :: Maybe (ProofState ReachabilityRule)
        }
    deriving (Show)

instance Pretty DebugProofState where
    pretty
        DebugProofState
            { proofState
            , transition
            , result
            }
      =
        Pretty.vsep
            [ "Reached proof state with the following configuration:"
            , Pretty.indent 4 (pretty proofState)
            , "On which the following transition applies:"
            , Pretty.indent 4 (prettyTransition transition)
            , "Resulting in:"
            , Pretty.indent 4 (maybe "Terminal state." pretty result)
            ]
      where
        prettyTransition (DeriveSeq _) = "Transition DeriveSeq."
        prettyTransition (DerivePar _) = "Transition DerivePar."
        prettyTransition prim          = Pretty.pretty prim

instance Entry DebugProofState where
    entrySeverity _ = Debug
    helpDoc _ = "log proof state"
