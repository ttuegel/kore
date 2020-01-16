{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}
module Kore.Step.Simplification.Condition
    ( create
    , simplify
    , simplifyPredicate
    , simplifyCondition
    ) where

import qualified Control.Monad.Trans as Monad.Trans
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified GHC.Stack as GHC

import Branch
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.Conditional as Conditional
import Kore.Internal.Pattern
    ( Condition
    , Conditional (..)
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , unwrapPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Step.Simplification.Simplify
import Kore.Step.Simplification.SubstitutionSimplifier
    ( SubstitutionSimplifier (..)
    )
import qualified Kore.TopBottom as TopBottom
import qualified Kore.Unification.Substitution as Substitution
import Kore.Unparser

{- | Create a 'ConditionSimplifier' using 'simplify'.
-}
create
    :: MonadSimplify simplifier
    => SubstitutionSimplifier simplifier
    -> ConditionSimplifier simplifier
create substitutionSimplifier =
    ConditionSimplifier $ simplify substitutionSimplifier

{- | Simplify a 'Condition'.

@simplify@ applies the substitution to the predicate and simplifies the
result. The result is re-simplified until it stabilizes.

The 'term' of 'Conditional' may be any type; it passes through @simplify@
unmodified.
-}
simplify
    ::  forall simplifier variable any
    .   ( GHC.HasCallStack
        , SimplifierVariable variable
        , MonadSimplify simplifier
        )
    =>  SubstitutionSimplifier simplifier
    ->  SideCondition variable
    ->  Conditional variable any
    ->  BranchT simplifier (Conditional variable any)
simplify SubstitutionSimplifier { simplifySubstitution } sideCondition initial =
    normalize initial >>= worker
  where
    worker Conditional { term, predicate, substitution } = do
        let substitution' = Substitution.toMap substitution
            predicate' = Predicate.substitute substitution' predicate
        simplified <- simplifyPredicate sideCondition predicate'
        TopBottom.guardAgainstBottom simplified
        let merged = simplified <> Condition.fromSubstitution substitution
        normalized <- normalize merged
        -- Check for full simplification *after* normalization. Simplification
        -- may have produced irrelevant substitutions that become relevant after
        -- normalization.
        if fullySimplified normalized
            then return normalized { term }
            else worker normalized { term }

    fullySimplified Conditional { predicate, substitution } =
        Predicate.isFreeOf predicate variables
      where
        variables = Substitution.variables substitution

    normalize
        ::  forall any'
        .   Conditional variable any'
        ->  BranchT simplifier (Conditional variable any')
    normalize conditional@Conditional { substitution } = do
        let conditional' = conditional { substitution = mempty }
        predicates' <- Monad.Trans.lift $
            simplifySubstitution sideCondition substitution
        predicate' <- Branch.scatter predicates'
        return $ Conditional.andCondition conditional' predicate'

{- | Simplify the 'Predicate' once.

@simplifyPredicate@ does not attempt to apply the resulting substitution and
re-simplify the result.

See also: 'simplify'

-}
simplifyPredicate
    ::  ( GHC.HasCallStack
        , SimplifierVariable variable
        , MonadSimplify simplifier
        )
    =>  SideCondition variable
    ->  Predicate variable
    ->  BranchT simplifier (Condition variable)
simplifyPredicate sideCondition predicate = do
    patternOr <-
        Monad.Trans.lift
        $ simplifyConditionalTermToOr sideCondition
        $ unwrapPredicate predicate
    -- Despite using Monad.Trans.lift above, we do not need to
    -- explicitly check for \bottom because patternOr is an OrPattern.
    scatter (eraseTerm <$> patternOr)
  where
    eraseTerm conditional
      | TopBottom.isTop (Pattern.term conditional)
      = Conditional.withoutTerm conditional
      | otherwise =
        (error . show . Pretty.vsep)
            [ "Expecting a \\top term, but found:"
            , unparse conditional
            ]
