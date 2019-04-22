{-|
Module      : Kore.Step.Substitution
Description : Tools for manipulating substitutions when doing Kore execution.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Substitution
    ( PredicateSubstitutionMerger (..)
    , createLiftedPredicatesAndSubstitutionsMerger
    , createPredicatesAndSubstitutionsMerger
    , createPredicatesAndSubstitutionsMergerExcept
    , mergePredicatesAndSubstitutions
    , mergePredicatesAndSubstitutionsExcept
    , normalize
    , normalizeExcept
    ) where

import           Control.Monad.Except
                 ( runExceptT, withExceptT )
import qualified Control.Monad.Morph as Monad.Morph
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import           GHC.Stack
                 ( HasCallStack )

import           Kore.AST.Common
                 ( SortedVariable )
import           Kore.AST.MetaOrObject
import           Kore.Attribute.Symbol
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Predicate.Predicate
                 ( Predicate, makeAndPredicate, makeMultipleAndPredicate,
                 substitutionToPredicate )
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Representation.PredicateSubstitution
                 ( PredicateSubstitution, Predicated (..) )
import qualified Kore.Step.Representation.PredicateSubstitution as PredicateSubstitution
import           Kore.Step.Simplification.Data
import qualified Kore.TopBottom as TopBottom
import           Kore.Unification.Data
                 ( UnificationProof (EmptyUnificationProof) )
import           Kore.Unification.Error
                 ( substitutionToUnifyOrSubError )
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.SubstitutionNormalization
                 ( normalizeSubstitution )
import           Kore.Unification.UnifierImpl
                 ( normalizeSubstitutionDuplication )
import           Kore.Unification.Unify
                 ( MonadUnify )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
import           Kore.Variables.Fresh

newtype PredicateSubstitutionMerger level variable m =
    PredicateSubstitutionMerger
    (  [Predicate level variable]
    -> [Substitution level variable]
    -> m (PredicateSubstitution level variable)
    )

-- | Normalize the substitution and predicate of 'expanded'.
normalize
    :: forall level variable term
    .   ( level ~ Object
        , MetaOrObject level
        , FreshVariable variable
        , SortedVariable variable
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , Unparse (variable level)
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> Predicated level variable term
    -> BranchT Simplifier (Predicated level variable term)
normalize
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    Predicated { term, predicate, substitution }
  = do
    -- We collect all the results here because we should promote the
    -- substitution to the predicate when there is an error on *any* branch.
    results <-
        Monad.Trans.lift
        $ runExceptT
        $ Monad.Unify.getUnifier
        $ gather
        $ normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            Predicated { term = (), predicate, substitution }
    case results of
        Right normal -> scatter (applyTerm <$> normal)
        Left _ ->
            return Predicated
                { term
                , predicate =
                    makeAndPredicate
                        predicate
                        (substitutionToPredicate substitution)
                , substitution = mempty
                }
  where
    applyTerm predicated = predicated { term }

normalizeExcept
    ::  ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , SortedVariable variable
        , FreshVariable variable
        , MonadUnify unifierM
        , unifier ~ unifierM variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> PredicateSubstitution level variable
    -> BranchT unifier (PredicateSubstitution level variable)
normalizeExcept
    tools
    predicateSimplifier@(PredicateSubstitutionSimplifier simplifySubstitution)
    simplifier
    axiomIdToSimplifier
    Predicated { predicate, substitution }
  = do
    -- The intermediate steps do not need to be checked for \bottom because we
    -- use guardAgainstBottom at the end.
    (deduplicated, _) <- normalizeSubstitutionDuplication' substitution
    let
        Predicated { substitution = preDeduplicatedSubstitution } = deduplicated
        Predicated { predicate = deduplicatedPredicate } = deduplicated
        -- The substitution is not fully normalized, but it is safe to convert
        -- to a Map because it has been deduplicated.
        deduplicatedSubstitution =
            Map.fromList $ Substitution.unwrap preDeduplicatedSubstitution

    normalized <- normalizeSubstitution' deduplicatedSubstitution
    let
        Predicated { substitution = normalizedSubstitution } = normalized
        Predicated { predicate = normalizedPredicate } = normalized

        mergedPredicate =
            makeMultipleAndPredicate
                [predicate, deduplicatedPredicate, normalizedPredicate]

    TopBottom.guardAgainstBottom mergedPredicate
    Monad.Morph.hoist Monad.Unify.liftSimplifier
        $ simplifySubstitution Predicated
            { term = ()
            , predicate = mergedPredicate
            , substitution = normalizedSubstitution
            }
  where
    normalizeSubstitutionDuplication' =
        Monad.Trans.lift
        . normalizeSubstitutionDuplication
            tools
            predicateSimplifier
            simplifier
            axiomIdToSimplifier

    normalizeSubstitution' =
        Monad.Trans.lift
        . Monad.Unify.fromExceptT
        . withExceptT substitutionToUnifyOrSubError
        . normalizeSubstitution tools

{-|'mergePredicatesAndSubstitutions' merges a list of substitutions into
a single one, then merges the merge side condition and the given condition list
into a condition.

If it does not know how to merge the substitutions, it will transform them into
predicates and redo the merge.

hs-boot: Please remember to update the hs-boot file when changing the signature.
-}
mergePredicatesAndSubstitutions
    :: ( Show (variable level)
       , Unparse (variable level)
       , SortedVariable variable
       , MetaOrObject level
       , Ord (variable level)
       , OrdMetaOrObject variable
       , ShowMetaOrObject variable
       , FreshVariable variable
       )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> [Predicate level variable]
    -> [Substitution level variable]
    -> Simplifier
        ( PredicateSubstitution level variable
        , UnificationProof level variable
        )
mergePredicatesAndSubstitutions
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    predicates
    substitutions
  = do
    result <- runExceptT . Monad.Unify.getUnifier $
        mergePredicatesAndSubstitutionsExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicates
            substitutions
    case result of
        Left _ ->
            let
                mergedPredicate =
                    makeMultipleAndPredicate
                        (  predicates
                        ++ map substitutionToPredicate substitutions
                        )
            in
                return
                    ( Predicated
                        { term = ()
                        , predicate = mergedPredicate
                        , substitution = mempty
                        }
                    , EmptyUnificationProof
                    )
        Right r -> return r

mergePredicatesAndSubstitutionsExcept
    ::  ( Show (variable level)
        , SortedVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , MonadUnify unifierM
        , unifier ~ unifierM variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> [Predicate level variable]
    -> [Substitution level variable]
    -> unifier
        ( PredicateSubstitution level variable
        , UnificationProof level variable
        )
mergePredicatesAndSubstitutionsExcept
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    predicates
    substitutions
  = do
    let
        mergedSubstitution = Foldable.fold substitutions
        mergedPredicate = makeMultipleAndPredicate predicates
    (Predicated {predicate, substitution}, _proof) <-
        normalizeSubstitutionAfterMerge
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            Predicated
                { term = ()
                , predicate = mergedPredicate
                , substitution = mergedSubstitution
                }
    return
        (Predicated
            { term = ()
            , predicate = predicate
            , substitution = substitution
            }
        , EmptyUnificationProof
        )

{-| Creates a 'PredicateSubstitutionMerger' that returns errors on unifications it
can't handle.
-}
createPredicatesAndSubstitutionsMergerExcept
    :: forall level variable unifier unifierM .
        ( Show (variable level)
        , Unparse (variable level)
        , SortedVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , MonadUnify unifierM
        , unifier ~ unifierM variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> PredicateSubstitutionMerger level variable unifier
createPredicatesAndSubstitutionsMergerExcept
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateSubstitutionMerger worker
  where
    worker
        :: [Predicate level variable]
        -> [Substitution level variable]
        -> unifier (PredicateSubstitution level variable)
    worker predicates substitutions = do
        (merged, _proof) <- mergePredicatesAndSubstitutionsExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicates
            substitutions
        return merged

{-| Creates a 'PredicateSubstitutionMerger' that creates predicates for
unifications it can't handle.
-}
createPredicatesAndSubstitutionsMerger
    :: forall level variable .
        ( Show (variable level)
        , Unparse (variable level)
        , SortedVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> PredicateSubstitutionMerger level variable Simplifier
createPredicatesAndSubstitutionsMerger
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateSubstitutionMerger worker
  where
    worker
        :: [Predicate level variable]
        -> [Substitution level variable]
        -> Simplifier (PredicateSubstitution level variable)
    worker predicates substitutions = do
        (merged, _proof) <- mergePredicatesAndSubstitutions
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicates
            substitutions
        return merged

{-| Creates a 'PredicateSubstitutionMerger' that creates predicates for
unifications it can't handle and whose result is in any monad transformer
over the base monad.
-}
createLiftedPredicatesAndSubstitutionsMerger
    :: forall level variable unifier unifierM .
        ( Show (variable level)
        , Unparse (variable level)
        , SortedVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , FreshVariable variable
        , MonadUnify unifierM
        , unifier ~ unifierM variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> PredicateSubstitutionMerger level variable unifier
createLiftedPredicatesAndSubstitutionsMerger
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateSubstitutionMerger worker
  where
    worker
        :: [Predicate level variable]
        -> [Substitution level variable]
        -> unifier (PredicateSubstitution level variable)
    worker predicates substitutions = Monad.Unify.liftSimplifier $ do
        (merged, _proof) <- mergePredicatesAndSubstitutions
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicates
            substitutions
        return merged

normalizeSubstitutionAfterMerge
    ::  ( MetaOrObject level
        , Ord (variable level)
        , Show (variable level)
        , Unparse (variable level)
        , OrdMetaOrObject variable
        , ShowMetaOrObject variable
        , SortedVariable variable
        , FreshVariable variable
        , HasCallStack
        , MonadUnify unifierM
        , unifier ~ unifierM variable
        )
    => MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -> BuiltinAndAxiomSimplifierMap level
    -> PredicateSubstitution level variable
    -> unifier
          ( PredicateSubstitution level variable
          , UnificationProof level variable
          )
normalizeSubstitutionAfterMerge
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    predicateSubstitution
  = do
    results <-
        gather
        $ normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicateSubstitution
    case Foldable.toList results of
        [] -> return
            ( PredicateSubstitution.bottom
            , EmptyUnificationProof
            )
        [normal] -> return (normal, EmptyUnificationProof)
        _ -> error "Not implemented: branching during normalization"
