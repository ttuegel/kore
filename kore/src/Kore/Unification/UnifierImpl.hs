{-|
Module      : Kore.Unification.UnifierImpl
Description : Datastructures and functionality for performing unification on
              Pure patterns
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Unification.UnifierImpl where

import qualified Control.Comonad.Trans.Cofree as Cofree
import           Control.Monad
                 ( foldM )
import           Control.Monad.Except
                 ( ExceptT (..) )
import           Data.Function
                 ( on )
import qualified Data.Functor.Foldable as Recursive
import           Data.List
                 ( foldl', groupBy, partition, sortBy )
import           Data.List.NonEmpty
                 ( NonEmpty (..) )

import           Kore.AST.Pure
import           Kore.IndexedModule.MetadataTools
import qualified Kore.Predicate.Predicate as Predicate
                 ( isFalse, makeAndPredicate, makeTruePredicate )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Pattern
import           Kore.Step.Representation.ExpandedPattern
                 ( ExpandedPattern, PredicateSubstitution, Predicated (..) )
import qualified Kore.Step.Representation.ExpandedPattern as Predicated
import qualified Kore.Step.Representation.ExpandedPattern as ExpandedPattern
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier (..), Simplifier,
                 StepPatternSimplifier )
import           Kore.Step.StepperAttributes
import           Kore.Unification.Data
import           Kore.Unification.Error
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

import {-# SOURCE #-} Kore.Step.Simplification.AndTerms
       ( termUnification )
import {-# SOURCE #-} Kore.Step.Substitution
       ( mergePredicatesAndSubstitutionsExcept )

{-# ANN simplifyUnificationProof ("HLint: ignore Use record patterns" :: String) #-}
simplifyUnificationProof
    :: UnificationProof level variable
    -> UnificationProof level variable
simplifyUnificationProof EmptyUnificationProof = EmptyUnificationProof
simplifyUnificationProof (CombinedUnificationProof []) =
    EmptyUnificationProof
simplifyUnificationProof (CombinedUnificationProof [a]) =
    simplifyUnificationProof a
simplifyUnificationProof (CombinedUnificationProof items) =
    case simplifyCombinedItems items of
        []  -> EmptyUnificationProof
        [a] -> a
        as  -> CombinedUnificationProof as
simplifyUnificationProof a@(ConjunctionIdempotency _) = a
simplifyUnificationProof a@(Proposition_5_24_3 _ _ _) = a
simplifyUnificationProof
    (AndDistributionAndConstraintLifting symbolOrAlias unificationProof)
  =
    AndDistributionAndConstraintLifting
        symbolOrAlias
        (simplifyCombinedItems unificationProof)
simplifyUnificationProof a@(SubstitutionMerge _ _ _) = a

simplifyCombinedItems
    :: [UnificationProof level variable] -> [UnificationProof level variable]
simplifyCombinedItems =
    foldr (addContents . simplifyUnificationProof) []
  where
    addContents
        :: UnificationProof level variable
        -> [UnificationProof level variable]
        -> [UnificationProof level variable]
    addContents EmptyUnificationProof  proofItems           = proofItems
    addContents (CombinedUnificationProof items) proofItems =
        items ++ proofItems
    addContents other proofItems = other : proofItems

simplifyAnds
    ::  forall level variable unifier.
        ( MetaOrObject level
        , Eq level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , SortedVariable variable
        , FreshVariable variable
        , unifier ~ ExceptT (UnificationOrSubstitutionError level variable)
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> NonEmpty (StepPattern level variable)
    -> unifier Simplifier
        (ExpandedPattern level variable, UnificationProof level variable)
simplifyAnds
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    patterns
  = do
    result <- foldM simplifyAnds' ExpandedPattern.top patterns
    if Predicate.isFalse . ExpandedPattern.predicate $ result
        then return ( ExpandedPattern.bottom, EmptyUnificationProof )
        else return ( result, EmptyUnificationProof )
  where
    simplifyAnds'
        :: ExpandedPattern level variable
        -> StepPattern level variable
        -> ExceptT
            ( UnificationOrSubstitutionError level variable )
            Simplifier
            ( ExpandedPattern level variable )
    simplifyAnds' intermediate pat =
        case Cofree.tailF (Recursive.project pat) of
            AndPattern And { andFirst = lhs, andSecond = rhs } ->
                foldM simplifyAnds' intermediate [lhs, rhs]
            _ -> do
                (result, _) <-
                    termUnification
                        tools
                        substitutionSimplifier
                        simplifier
                        axiomIdToSimplifier
                        (ExpandedPattern.term intermediate)
                        pat
                (predSubst, _) <-
                    mergePredicatesAndSubstitutionsExcept
                        tools
                        substitutionSimplifier
                        simplifier
                        axiomIdToSimplifier
                        [ ExpandedPattern.predicate result
                        , ExpandedPattern.predicate intermediate
                        ]
                        [ ExpandedPattern.substitution result
                        , ExpandedPattern.substitution intermediate
                        ]
                return ExpandedPattern.Predicated
                    { term = ExpandedPattern.term result
                    , predicate = Predicated.predicate predSubst
                    , substitution = Predicated.substitution predSubst
                    }


groupSubstitutionByVariable
    :: Ord (variable level)
    => [(variable level, StepPattern level variable)]
    -> [[(variable level, StepPattern level variable)]]
groupSubstitutionByVariable =
    groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map sortRenaming
  where
    sortRenaming (var, Recursive.project -> ann :< VariablePattern var')
        | var' < var =
          (var', Recursive.embed (ann :< VariablePattern var))
    sortRenaming eq = eq

-- simplifies x = t1 /\ x = t2 /\ ... /\ x = tn by transforming it into
-- x = ((t1 /\ t2) /\ (..)) /\ tn
-- then recursively reducing that to finally get x = t /\ subst
solveGroupedSubstitution
    :: ( MetaOrObject level
       , Eq level
       , Ord (variable level)
       , Show (variable level)
       , Unparse (variable level)
       , OrdMetaOrObject variable
       , ShowMetaOrObject variable
       , SortedVariable variable
       , FreshVariable variable
       )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> variable level
    -> NonEmpty (StepPattern level variable)
    -> ExceptT
        ( UnificationOrSubstitutionError level variable )
        Simplifier
        ( PredicateSubstitution level variable
        , UnificationProof level variable
        )
solveGroupedSubstitution
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    var
    patterns
  = do
    (predSubst, proof) <-
        simplifyAnds
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            patterns
    return
        ( Predicated
            { term = ()
            , predicate = ExpandedPattern.predicate predSubst
            , substitution = Substitution.wrap $ termAndSubstitution predSubst
            }
        , proof
        )
  where
    termAndSubstitution s =
        (var, ExpandedPattern.term s)
        : Substitution.unwrap (ExpandedPattern.substitution s)

-- |Takes a potentially non-normalized substitution,
-- and if it contains multiple assignments to the same variable,
-- it solves all such assignments.
-- As new assignments may be produced during the solving process,
-- `normalizeSubstitutionDuplication` recursively calls itself until it
-- stabilizes.
normalizeSubstitutionDuplication
    :: forall variable level
    .   ( MetaOrObject level
        , Eq level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , SortedVariable variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> Substitution level variable
    -> ExceptT
        ( UnificationOrSubstitutionError level variable )
        Simplifier
        ( PredicateSubstitution level variable
        , UnificationProof level variable
        )
normalizeSubstitutionDuplication
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    subst
  =
    if null nonSingletonSubstitutions || Substitution.isNormalized subst
        then return
            ( Predicated () Predicate.makeTruePredicate subst
            , EmptyUnificationProof
            )
        else do
            (predSubst, proof') <-
                mergePredicateSubstitutionList
                <$> mapM
                    (uncurry
                        $ solveGroupedSubstitution
                            tools
                            substitutionSimplifier
                            simplifier
                            axiomIdToSimplifier
                    )
                    varAndSubstList
            (finalSubst, proof) <-
                normalizeSubstitutionDuplication
                    tools
                    substitutionSimplifier
                    simplifier
                    axiomIdToSimplifier
                    (  Substitution.wrap (concat singletonSubstitutions)
                    <> Predicated.substitution predSubst
                    )
            let
                pred' =
                    Predicate.makeAndPredicate
                        (Predicated.predicate predSubst)
                        (Predicated.predicate finalSubst)
            return
                ( Predicated
                    { term = ()
                    , predicate = pred'
                    , substitution = Predicated.substitution finalSubst
                    }
                , CombinedUnificationProof
                    [ proof'
                    , proof
                    ]
                )
  where
    groupedSubstitution = groupSubstitutionByVariable $ Substitution.unwrap subst
    isSingleton [_] = True
    isSingleton _   = False
    singletonSubstitutions, nonSingletonSubstitutions
        :: [[(variable level, StepPattern level variable)]]
    (singletonSubstitutions, nonSingletonSubstitutions) =
        partition isSingleton groupedSubstitution
    varAndSubstList :: [(variable level, NonEmpty (StepPattern level variable))]
    varAndSubstList =
        nonSingletonSubstitutions >>= \case
            [] -> []
            ((x, y) : ys) -> [(x, y :| (snd <$> ys))]


mergePredicateSubstitutionList
    :: ( MetaOrObject level
       , Eq level
       , Ord (variable level)
       , OrdMetaOrObject variable
       , SortedVariable variable
       , Show (variable level)
       , Unparse (variable level)
       )
    => [(PredicateSubstitution level variable, UnificationProof level variable)]
    -> (PredicateSubstitution level variable, UnificationProof level variable)
mergePredicateSubstitutionList [] =
    ( Predicated.topPredicate
    , EmptyUnificationProof
    )
mergePredicateSubstitutionList (p:ps) =
    foldl' mergePredicateSubstitutions p ps
  where
    mergePredicateSubstitutions
        ( Predicated { predicate = p1, substitution = s1 }, proofs)
        ( Predicated { predicate = p2, substitution = s2 }, proof) =
        ( Predicated
            { term = ()
            , predicate = Predicate.makeAndPredicate p1 p2
            , substitution = s1 <> s2
            }
        , proofs <> proof
        )
