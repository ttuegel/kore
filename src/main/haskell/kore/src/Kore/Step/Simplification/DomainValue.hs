{-|
Module      : Kore.Simplification.DomainValue
Description : Tools for DomainValue pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.DomainValue
    ( simplify
    ) where

import           Kore.AST.Common
                 ( BuiltinDomain, DomainValue (..),
                 Pattern (DomainValuePattern) )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( CommonPurePattern, asPurePattern )
import           Kore.Predicate.Predicate
                 ( makeTruePredicate )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern (ExpandedPattern) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
                 ( ExpandedPattern (..) )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( make )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof (..) )

{-| 'simplify' simplifies a 'DomainValue' pattern, which means returning
an or containing a term made of that value.
-}
simplify
    :: DomainValue Object (BuiltinDomain (CommonPurePattern Meta))
    -> ( OrOfExpandedPattern Object variable
       , SimplificationProof Object
       )
simplify dv =
    ( OrOfExpandedPattern.make
        [ExpandedPattern
            { term = asPurePattern (DomainValuePattern dv)
            , predicate = makeTruePredicate
            , substitution = []
            }
        ]
    , SimplificationProof
    )
