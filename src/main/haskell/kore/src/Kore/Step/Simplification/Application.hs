{-|
Module      : Kore.Step.Simplification.Application
Description : Tools for Application pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Application
    ( simplify
    ) where

import qualified Data.Map as Map

import           Kore.AST.Common
                 ( Application (..), Id, PureMLPattern, SortedVariable,
                 SymbolOrAlias )
import           Kore.AST.MetaOrObject
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import qualified Kore.IndexedModule.MetadataTools as HeadType
                 ( HeadType (..) )
import qualified Kore.IndexedModule.MetadataTools as MetadataTools
                 ( MetadataTools (..) )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..) )
import           Kore.Step.ExpandedPattern as ExpandedPattern
                 ( Predicated (..) )
import           Kore.Step.Function.Data
                 ( ApplicationFunctionEvaluator (..) )
import           Kore.Step.Function.Evaluator
                 ( evaluateApplication )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( fullCrossProduct, traverseFlattenWithPairsGeneric )
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier,
                 PureMLPatternSimplifier (..), SimplificationProof (..),
                 Simplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes (..) )
import           Kore.Step.Substitution
                 ( mergePredicatesAndSubstitutions )
import           Kore.Substitution.Class
                 ( Hashable )
import           Kore.Variables.Fresh

-- data ExpandedApplication level variable = ExpandedApplication
--     { term         :: !(Application level (PureMLPattern level variable))
--     , predicate    :: !(Predicate level variable)
--     , substitution :: !(UnificationSubstitution level variable)
--     }
--     deriving (Eq, Show)

type ExpandedApplication level variable =
    Predicated level variable (Application level (PureMLPattern level variable))

{-|'simplify' simplifies an 'Application' of 'OrOfExpandedPattern'.

To do that, it first distributes the terms, making it an Or of Application
patterns, each Application having 'ExpandedPattern's as children,
then it simplifies each of those.

Simplifying an Application of ExpandedPattern means merging the children
predicates ans substitutions, applying functions on the Application(terms),
then merging everything into an ExpandedPattern.
-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Show (variable Meta)
        , Show (variable Object)
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions.
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> Application level (OrOfExpandedPattern level variable)
    -> Simplifier
        ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplify
    tools
    substitutionSimplifier
    simplifier
    symbolIdToEvaluator
    Application
        { applicationSymbolOrAlias = symbol
        , applicationChildren = children
        }
  = do
    let
        -- The "Propagation Or" inference rule together with
        -- "Propagation Bottom" for the case when a child or is empty.
        orDistributedChildren = OrOfExpandedPattern.fullCrossProduct children
    (unflattenedOr, _proofs) <-
        OrOfExpandedPattern.traverseFlattenWithPairsGeneric
            (makeAndEvaluateApplications
                tools substitutionSimplifier simplifier symbolIdToEvaluator
                symbol
            )
            orDistributedChildren
    return
        ( unflattenedOr
        , SimplificationProof
        )

makeAndEvaluateApplications
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Show (variable Meta)
        , Show (variable Object)
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions.
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> SymbolOrAlias level
    -> [ExpandedPattern level variable]
    -> Simplifier
        (OrOfExpandedPattern level variable, SimplificationProof level)
makeAndEvaluateApplications
    tools
    substitutionSimplifier
    simplifier
    symbolIdToEvaluator
    symbol
    children
  =
    case MetadataTools.symbolOrAliasType tools symbol of
        HeadType.Symbol ->
            makeAndEvaluateSymbolApplications
                tools
                substitutionSimplifier
                simplifier
                symbolIdToEvaluator
                symbol
                children
        HeadType.Alias -> error "Alias evaluation not implemented yet."

makeAndEvaluateSymbolApplications
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Show (variable Meta)
        , Show (variable Object)
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions.
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> SymbolOrAlias level
    -> [ExpandedPattern level variable]
    -> Simplifier
        (OrOfExpandedPattern level variable, SimplificationProof level)
makeAndEvaluateSymbolApplications
    tools
    substitutionSimplifier
    simplifier
    symbolIdToEvaluator
    symbol
    children
  = do
    (expandedApplication, _proof) <-
        makeExpandedApplication tools substitutionSimplifier symbol children
    (functionApplication, _proof) <-
        evaluateApplicationFunction
            tools substitutionSimplifier simplifier symbolIdToEvaluator
            expandedApplication
    return (functionApplication, SimplificationProof)

evaluateApplicationFunction
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Show (variable Meta)
        , Show (variable Object)
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> PureMLPatternSimplifier level variable
    -- ^ Evaluates functions.
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level]
    -- ^ Map from symbol IDs to defined functions
    -> ExpandedApplication level variable
    -- ^ The pattern to be evaluated
    -> Simplifier
        (OrOfExpandedPattern level variable, SimplificationProof level)
evaluateApplicationFunction
    tools
    substitutionSimplifier
    simplifier
    symbolIdToEvaluator
    Predicated
        { term, predicate, substitution }
  =
    evaluateApplication
        tools
        substitutionSimplifier
        simplifier
        symbolIdToEvaluator
        Predicated { term = (), predicate, substitution }
        term

makeExpandedApplication
    ::  ( MetaOrObject level
        , Kore.AST.Common.SortedVariable variable
        , Ord (variable level)
        , Show (variable level)
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
    -> SymbolOrAlias level
    -> [ExpandedPattern level variable]
    -> Simplifier
        (ExpandedApplication level variable, SimplificationProof level)
makeExpandedApplication tools substitutionSimplifier symbol children
  = do
    (   Predicated
            { predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , _proof) <-
            mergePredicatesAndSubstitutions
                tools
                substitutionSimplifier
                (map ExpandedPattern.predicate children)
                (map ExpandedPattern.substitution children)
    return
        ( Predicated
            { term = Application
                { applicationSymbolOrAlias = symbol
                , applicationChildren = map ExpandedPattern.term children
                }
            , predicate = mergedPredicate
            , substitution = mergedSubstitution
            }
        , SimplificationProof
        )
