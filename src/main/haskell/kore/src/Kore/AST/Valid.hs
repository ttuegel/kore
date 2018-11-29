{-|
Module      : Kore.AST.Valid
Description : Annotations collected during verification
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.AST.Valid where

import Kore.AST.Common
       ( Sort )

{- | @Valid@ is a pattern annotation of facts collected during verification.
 -}
data Valid level =
    Valid
        { patternSort :: !(Sort level)
            -- ^ The sort determined by the verifier.
        }
    deriving (Eq, Ord, Show)
