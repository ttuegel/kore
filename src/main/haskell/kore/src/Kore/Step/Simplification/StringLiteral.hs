{-|
Module      : Kore.Step.Simplification.StringLiteral
Description : Tools for StringLiteral pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.StringLiteral
    ( simplify
    ) where

import           Kore.AST.Common
                 ( Pattern (StringLiteralPattern), StringLiteral (..) )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( asPurePattern )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( Predicated (..) )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )

{-| 'simplify' simplifies a 'StringLiteral' pattern, which means returning
an or containing a term made of that literal.
-}
simplify
    :: StringLiteral
    -> ( OrOfExpandedPattern Meta variable
       , SimplificationProof Meta
       )
simplify str =
    ( OrOfExpandedPattern.make
        [Predicated
            { term = asPurePattern (StringLiteralPattern str)
            , predicate = makeTruePredicate
            , substitution = []
            }
        ]
    , SimplificationProof
    )
