{-|
Module      : Kore.Substitute
Description : Abstract substitution algorithm
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
-}

module Kore.Substitute
    ( substitute
    ) where

import           Control.Comonad
import qualified Control.Lens as Lens
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Foldable as Recursive
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Set
                 ( Set )
import qualified Data.Set as Set

import Control.Monad.Counter
       ( MonadCounter )
import Kore.AST.Common
       ( Exists (..), Forall (..), Pattern (..), SortedVariable )
import Kore.AST.MetaOrObject
import Kore.AST.Pure
import Kore.Variables.Fresh
       ( FreshVariable, freshVariableSuchThat )

{- | Traverse the pattern from the top down and apply substitutions.

The 'freeVariables' annotation is used to avoid traversing subterms that
contain none of the targeted variables.

The substitution must be normalized, i.e. no target (left-hand side) variable
may appear in the right-hand side of any substitution, but this is not checked.

 -}
-- TODO (thomas.tuegel): In the future, patterns may have other types of
-- attributes which need to be re-synthesized after substitution.
substitute
    ::  forall m level domain variable attribute.
        ( FreshVariable variable
        , MetaOrObject level
        , MonadCounter m
        , Ord (variable level)
        , SortedVariable variable
        , Traversable domain
        )
    => Lens.Lens' attribute (Set (variable level))
    -- ^ Lens into free variables of the pattern
    -> Map (variable level) (PurePattern level domain variable attribute)
    -- ^ Substitution
    -> PurePattern level domain variable attribute
    -- ^ Original pattern
    -> m (PurePattern level domain variable attribute)
substitute lensFreeVariables = \subst -> substituteWorker (Map.map Right subst)
  where
    extractFreeVariables
        :: PurePattern level domain variable attribute
        -> Set (variable level)
    extractFreeVariables = Lens.view lensFreeVariables . extract
    substituteWorker subst stepPattern
      | Map.null subst' =
        -- If there are no targeted free variables, return the original pattern.
        -- Note that this covers the case of a non-targeted variable pattern,
        -- which produces an error below.
        return stepPattern
      | otherwise =
        case stepPatternHead of
            -- Capturing quantifiers
            ExistsPattern exists@Exists { existsVariable }
              | Set.member existsVariable targetFreeVariables -> do
                exists' <- substituteUnderExists exists
                (return . Recursive.embed) (attrib' :< ExistsPattern exists')

            ForallPattern forall@Forall { forallVariable }
              | Set.member forallVariable targetFreeVariables -> do
                forall' <- substituteUnderForall forall
                (return . Recursive.embed) (attrib' :< ForallPattern forall')

            -- Variables
            VariablePattern variable ->
                case Map.lookup variable subst' of
                    Nothing ->
                        -- This is impossible: if the pattern is a non-targeted
                        -- variable, we would have taken the first branch at
                        -- the top of substituteWorker.
                        error "Internal error: Impossible free variable"
                    Just (Left variable') ->
                        (return . Recursive.embed)
                            (attrib' :< VariablePattern variable')
                    Just (Right stepPattern') ->
                        return stepPattern'

            -- All other patterns
            _ -> do
                stepPatternHead' <-
                    traverse (substituteWorker subst') stepPatternHead
                (return . Recursive.embed) (attrib' :< stepPatternHead')
      where
        attrib :< stepPatternHead = Recursive.project stepPattern
        freeVariables = Lens.view lensFreeVariables attrib
        attrib' = Lens.set lensFreeVariables freeVariables' attrib
        -- | The substitution applied to subterms, including only the free
        -- variables below the current node. Shadowed variables are
        -- automatically omitted.
        subst' = Map.intersection subst (Map.fromSet id freeVariables)
        -- | Free variables of the original pattern that are not targeted.
        originalVariables = Set.difference freeVariables (Map.keysSet subst')
        -- | Free variables of the target substitutions.
        targetFreeVariables =
            Foldable.foldl'
                Set.union
                Set.empty
                (either Set.singleton extractFreeVariables <$> subst')
        freeVariables' = Set.union originalVariables targetFreeVariables

        {- | Perform capture-avoiding substitution under a binder by renaming.

        The bound variable is freshened with respect to the set of free
        variables and the given substitution (along with renaming) is applied to
        the child pattern.  The result is the variable and child pattern after
        renaming.

        -}
        substituteUnderBinder variable child = do
            variable' <- freshVariableSuchThat variable wouldNotCapture
            -- Rename the freshened bound variable in the subterms.
            let subst'' = Map.insert variable (Left variable') subst
            child' <- substituteWorker subst'' child
            return (variable', child')
          where
            wouldNotCapture variable' = Set.notMember variable' freeVariables'

        {- | Perform capture-avoiding substitution under 'Exists' by renaming.

        The bound variable is freshened with respect to the set of free
        variables and the given substitution (along with renaming) is applied to
        the child pattern.  The result is the 'Exists' binder after renaming.

        -}
        substituteUnderExists exists = do
            (existsVariable', existsChild') <-
                substituteUnderBinder existsVariable existsChild
            return exists
                { existsVariable = existsVariable'
                , existsChild = existsChild'
                }
          where
            Exists { existsVariable, existsChild } = exists

        {- | Perform capture-avoiding substitution under 'Forall' by renaming.

        The bound variable is freshened with respect to the set of free
        variables and the given substitution (along with renaming) is applied to
        the child pattern.  The result is the 'Forall' binder after renaming.

        -}
        substituteUnderForall forall = do
            (forallVariable', forallChild') <-
                substituteUnderBinder forallVariable forallChild
            return forall
                { forallVariable = forallVariable'
                , forallChild = forallChild'
                }
          where
            Forall { forallVariable, forallChild } = forall

{-# INLINE substitute #-}
