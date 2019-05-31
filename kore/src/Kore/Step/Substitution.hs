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
    ( PredicateMerger (..)
    , createLiftedPredicatesAndSubstitutionsMerger
    , createPredicatesAndSubstitutionsMerger
    , createPredicatesAndSubstitutionsMergerExcept
    , mergePredicatesAndSubstitutions
    , mergePredicatesAndSubstitutionsExcept
    , normalize
    , normalizeExcept
    ) where

import           Control.Monad.Except
                 ( withExceptT )
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map
import           GHC.Stack
                 ( HasCallStack )

import           Kore.Attribute.Symbol
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import           Kore.Internal.Predicate
                 ( Conditional (..), Predicate )
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Predicate.Predicate as Syntax
                 ( Predicate )
import qualified Kore.Predicate.Predicate as Syntax.Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.Data as BranchT
                 ( scatter )
import           Kore.Syntax.Variable
                 ( SortedVariable )
import qualified Kore.TopBottom as TopBottom
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

newtype PredicateMerger variable m =
    PredicateMerger
    (  [Syntax.Predicate variable]
    -> [Substitution variable]
    -> m (Predicate variable)
    )

-- | Normalize the substitution and predicate of 'expanded'.
normalize
    :: forall variable term
    .   ( FreshVariable variable
        , SortedVariable variable
        , Unparse variable
        , Show variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Conditional variable term
    -> BranchT Simplifier (Conditional variable term)
normalize
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    Conditional { term, predicate, substitution }
  = do
    -- We collect all the results here because we should promote the
    -- substitution to the predicate when there is an error on *any* branch.
    results <-
        Monad.Trans.lift
        $ Monad.Unify.runUnifier
        $ normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            Conditional { term = (), predicate, substitution }
    case results of
        Right normal -> scatter (applyTerm <$> normal)
        Left _ ->
            return Conditional
                { term
                , predicate =
                    Syntax.Predicate.makeAndPredicate predicate
                    $ Syntax.Predicate.fromSubstitution substitution
                , substitution = mempty
                }
  where
    applyTerm predicated = predicated { term }

normalizeExcept
    ::  ( Ord variable
        , Show variable
        , Unparse variable
        , SortedVariable variable
        , FreshVariable variable
        , MonadUnify unifier
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Predicate variable
    -> unifier (Predicate variable)
normalizeExcept
    _tools
    predicateSimplifier@(PredicateSimplifier simplifySubstitution)
    simplifier
    axiomIdToSimplifier
    Conditional { predicate, substitution }
  = do
    -- The intermediate steps do not need to be checked for \bottom because we
    -- use guardAgainstBottom at the end.
    deduplicated <- normalizeSubstitutionDuplication' substitution
    let
        Conditional { substitution = preDeduplicatedSubstitution } =
            deduplicated
        Conditional { predicate = deduplicatedPredicate } = deduplicated
        -- The substitution is not fully normalized, but it is safe to convert
        -- to a Map because it has been deduplicated.
        deduplicatedSubstitution =
            Map.fromList $ Substitution.unwrap preDeduplicatedSubstitution

    normalized <- normalizeSubstitution' deduplicatedSubstitution
    let
        Conditional { substitution = normalizedSubstitution } = normalized
        Conditional { predicate = normalizedPredicate } = normalized

        mergedPredicate =
            Syntax.Predicate.makeMultipleAndPredicate
                [predicate, deduplicatedPredicate, normalizedPredicate]

    TopBottom.guardAgainstBottom mergedPredicate
    Monad.Unify.liftBranchedSimplifier
        $ simplifySubstitution Conditional
            { term = ()
            , predicate = mergedPredicate
            , substitution = normalizedSubstitution
            }
  where
    normalizeSubstitutionDuplication' =
        normalizeSubstitutionDuplication
            predicateSimplifier
            simplifier
            axiomIdToSimplifier

    normalizeSubstitution' =
        Monad.Unify.fromExceptT
        . withExceptT substitutionToUnifyOrSubError
        . normalizeSubstitution

{-|'mergePredicatesAndSubstitutions' merges a list of substitutions into
a single one, then merges the merge side condition and the given condition list
into a condition.

If it does not know how to merge the substitutions, it will transform them into
predicates and redo the merge.

hs-boot: Please remember to update the hs-boot file when changing the signature.
-}
mergePredicatesAndSubstitutions
    :: ( Show variable
       , Unparse variable
       , SortedVariable variable
       , Ord variable
       , FreshVariable variable
       , HasCallStack
       )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> [Syntax.Predicate variable]
    -> [Substitution variable]
    -> BranchT Simplifier (Predicate variable)
mergePredicatesAndSubstitutions
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    predicates
    substitutions
  = do
    result <- Monad.Trans.lift $ Monad.Unify.runUnifier $
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
                    Syntax.Predicate.makeMultipleAndPredicate
                        (  predicates
                        ++ map Syntax.Predicate.fromSubstitution substitutions
                        )
            in
                return $ Predicate.fromPredicate mergedPredicate
        Right r -> BranchT.scatter r

mergePredicatesAndSubstitutionsExcept
    ::  ( Show variable
        , SortedVariable variable
        , Ord variable
        , Unparse variable
        , FreshVariable variable
        , HasCallStack
        , MonadUnify unifier
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> [Syntax.Predicate variable]
    -> [Substitution variable]
    -> unifier (Predicate variable)
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
        mergedPredicate = Syntax.Predicate.makeMultipleAndPredicate predicates
    normalizeSubstitutionAfterMerge
        tools
        substitutionSimplifier
        simplifier
        axiomIdToSimplifier
        Conditional
            { term = ()
            , predicate = mergedPredicate
            , substitution = mergedSubstitution
            }

{-| Creates a 'PredicateMerger' that returns errors on unifications it
can't handle.
-}
createPredicatesAndSubstitutionsMergerExcept
    ::  forall variable unifier
    .   ( Show variable
        , Unparse variable
        , SortedVariable variable
        , Ord variable
        , FreshVariable variable
        , MonadUnify unifier
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> PredicateMerger variable unifier
createPredicatesAndSubstitutionsMergerExcept
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateMerger worker
  where
    worker
        :: [Syntax.Predicate variable]
        -> [Substitution variable]
        -> unifier (Predicate variable)
    worker predicates substitutions = do
        let merged =
                (Predicate.fromPredicate <$> predicates)
                <> (Predicate.fromSubstitution <$> substitutions)
        normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            (Foldable.fold merged)

{-| Creates a 'PredicateMerger' that creates predicates for
unifications it can't handle.
-}
createPredicatesAndSubstitutionsMerger
    :: forall variable .
        ( Show variable
        , Unparse variable
        , SortedVariable variable
        , Ord variable
        , FreshVariable variable
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> PredicateMerger variable (BranchT Simplifier)
createPredicatesAndSubstitutionsMerger
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateMerger worker
  where
    worker
        :: [Syntax.Predicate variable]
        -> [Substitution variable]
        -> BranchT Simplifier (Predicate variable)
    worker predicates substitutions = do
        let merged =
                (Predicate.fromPredicate <$> predicates)
                <> (Predicate.fromSubstitution <$> substitutions)
        normalize
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            (Foldable.fold merged)

{-| Creates a 'PredicateMerger' that creates predicates for
unifications it can't handle and whose result is in any monad transformer
over the base monad.
-}
createLiftedPredicatesAndSubstitutionsMerger
    ::  forall variable unifier
    .   ( Show variable
        , Unparse variable
        , SortedVariable variable
        , Ord variable
        , FreshVariable variable
        , MonadUnify unifier
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> PredicateMerger variable unifier
createLiftedPredicatesAndSubstitutionsMerger
    tools substitutionSimplifier simplifier axiomIdToSimplifier
  =
    PredicateMerger worker
  where
    worker
        :: [Syntax.Predicate variable]
        -> [Substitution variable]
        -> unifier (Predicate variable)
    worker predicates substitutions = do
        let merged =
                (Predicate.fromPredicate <$> predicates)
                <> (Predicate.fromSubstitution <$> substitutions)
        normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            (Foldable.fold merged)

normalizeSubstitutionAfterMerge
    ::  ( Ord variable
        , Show variable
        , Unparse variable
        , SortedVariable variable
        , FreshVariable variable
        , HasCallStack
        , MonadUnify unifier
        )
    => SmtMetadataTools StepperAttributes
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Predicate variable
    -> unifier (Predicate variable)
normalizeSubstitutionAfterMerge
    tools
    substitutionSimplifier
    simplifier
    axiomIdToSimplifier
    predicate
  = do
    results <-
        Monad.Unify.gather
        $ normalizeExcept
            tools
            substitutionSimplifier
            simplifier
            axiomIdToSimplifier
            predicate
    case Foldable.toList results of
        [] -> return Predicate.bottom
        [normal] -> return normal
        -- TODO(virgil): Allow multiple results and consider dropping this
        -- function.
        _ -> error "Not implemented: branching during normalization"
