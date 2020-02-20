{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Log.DebugInitializeClaim
    ( DebugInitializeClaim (..)
    , debugInitializeClaim
    ) where

import Prelude.Kore

import qualified Data.Foldable as Foldable
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Internal.MultiAnd
    ( MultiAnd
    )
import Kore.Internal.Variable
import Kore.Step.RulePattern
    ( ReachabilityRule (..)
    )
import Kore.Unparser
import Log
import Pretty
    ( Pretty (..)
    )
import qualified Pretty
import qualified SQL

{- | A log 'Entry' when a claim is initialized.

 -}
data DebugInitializeClaim =
    DebugInitializeClaim
        { originalClaim :: ReachabilityRule Variable
        , initializedClaims :: MultiAnd (ReachabilityRule Variable)
        }
    deriving (Eq)
    deriving (GHC.Generic)

instance SOP.Generic DebugInitializeClaim

instance SOP.HasDatatypeInfo DebugInitializeClaim

instance Entry DebugInitializeClaim where
    entrySeverity _ = Debug

instance Pretty DebugInitializeClaim where
    pretty entry@(DebugInitializeClaim _ _) =
        Pretty.vsep
            [ "Initialized claim:"
            , (Pretty.indent 2 . unparse) originalClaim
            , "producing simplified claims:"
            , (Pretty.indent 2 . Pretty.vsep)
                (unparse <$> Foldable.toList initializedClaims)
            ]
      where
        DebugInitializeClaim { originalClaim } = entry
        DebugInitializeClaim { initializedClaims } = entry

instance SQL.Table DebugInitializeClaim

{- | Log the 'DebugInitializeClaim' entry.
 -}
debugInitializeClaim
    :: MonadLog log
    => ReachabilityRule Variable
    -> MultiAnd (ReachabilityRule Variable)
    -> log ()
debugInitializeClaim originalClaim initializedClaims =
    logEntry DebugInitializeClaim { originalClaim, initializedClaims }
