{-|
Module      : Kore.Step.Simplification.DomainValue
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

import Data.Reflection
       ( give )
import Prelude hiding
       ( pred )

import Kore.AST.Common
       ( BuiltinDomain (..), DomainValue (..), Pattern (DomainValuePattern),
       SortedVariable )

import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( asPurePattern )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Simplification.Data
import           Kore.Step.Simplifier
import           Kore.Step.StepperAttributes
import           Kore.Substitution.Class
                 ( Hashable )
import           Kore.Variables.Fresh

{-| 'simplify' simplifies a 'DomainValue' pattern, which means returning
an or containing a term made of that value.
-}
simplify
    :: ( OrdMetaOrObject variable, ShowMetaOrObject variable
       , SortedVariable variable
       , Hashable variable
       , FreshVariable variable
       )
    => MetadataTools Object StepperAttributes
    -> DomainValue Object (BuiltinDomain (OrOfExpandedPattern Object variable))
    -> Simplifier
        ( OrOfExpandedPattern Object variable
        , SimplificationProof Object
        )
simplify
    tools@MetadataTools { symbolOrAliasSorts }
    DomainValue { domainValueSort, domainValueChild }
  = do
    patts <-
        give symbolOrAliasSorts
        $ sequence
        $ do
            builtin <- sequence domainValueChild
            (return . ExpandedPattern.normalizeSubstitution tools)
                (asDomainValue <$> sequenceA builtin)
    return (OrOfExpandedPattern.filterOr patts, SimplificationProof)
  where
    asDomainValue domainValueChild' =
        (asPurePattern . DomainValuePattern)
            DomainValue
                { domainValueSort
                , domainValueChild = domainValueChild'
                }
