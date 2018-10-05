{-|
Module      : Kore.Step.Condition.Evaluator
Description : Evaluates conditions.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Condition.Evaluator
    ( evaluate
    ) where

import Data.Reflection

import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.ASTUtils.SmartPatterns
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SymbolOrAliasSorts )
import           Kore.Predicate.Predicate
                 ( Predicate, makeAndPredicate, makeFalsePredicate,
                 unwrapPredicate, wrapPredicate )
import           Kore.SMT.SMT
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, PredicateSubstitution, Predicated (..) )
import           Kore.Step.ExpandedPattern as PredicateSubstitution
                 ( PredicateSubstitution (..) )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( toExpandedPattern )
import           Kore.Step.Simplification.Data
                 ( PureMLPatternSimplifier (..),
                 SimplificationProof (SimplificationProof), Simplifier )
import           Kore.Step.StepperAttributes

convertStepperToSMT
    :: MetadataTools level StepperAttributes
    -> MetadataTools level SMTAttributes
convertStepperToSMT tools =
    tools
    { symAttributes  = convert . symAttributes  tools
    , sortAttributes = convert . sortAttributes tools
    , isSubsortOf = const $ const False -- no subsort info needed by SMT
    }
    where convert (StepperAttributes _ _ _ _ _ hook) = SMTAttributes hook

-- TODO: May add more checks later
-- but the vast majority of predicates are just `top`.
nonTrivial :: PureMLPattern level variable -> Bool
nonTrivial (Top_ _) = False
nonTrivial _ = True

{-| 'evaluate' attempts to evaluate a Kore predicate. -}
evaluate
    ::  forall level variable .
        ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable variable
        , Eq (variable level)
        , Ord (variable level)
        , Show (variable level)
        , Given (MetadataTools level StepperAttributes)
        )
    => PureMLPatternSimplifier level variable
    -- ^ Evaluates functions in a pattern.
    -> Predicate level variable
    -- ^ The condition to be evaluated.
    -- TODO: Can't it happen that I also get a substitution when evaluating
    -- functions? See the Equals case.
    -> Simplifier
        (PredicateSubstitution level variable, SimplificationProof level)
evaluate
    (PureMLPatternSimplifier simplifier)
    predicate''
  = give (convertStepperToSMT (given :: MetadataTools level StepperAttributes))
    $ do
    let predicate' =
            if nonTrivial (unwrapPredicate predicate'')
               && (unsafeTryRefutePredicate predicate'')
                   == Just False
            then makeFalsePredicate
            else predicate''
    (patt, _proof) <- simplifier (unwrapPredicate predicate')
    let
        (subst, _proof) =
            asPredicateSubstitution (OrOfExpandedPattern.toExpandedPattern patt)
    return ( subst, SimplificationProof)

asPredicateSubstitution
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable variable
        , Eq (variable level)
        , Show (variable level)
        )
    => ExpandedPattern level variable
    -> (PredicateSubstitution level variable, SimplificationProof level)
asPredicateSubstitution
    Predicated {term, predicate, substitution}
  =
    let
        (andPatt, _proof) = makeAndPredicate predicate (wrapPredicate term)
    in
        ( PredicateSubstitution
            { predicate = andPatt
            , substitution = substitution
            }
        , SimplificationProof
        )
