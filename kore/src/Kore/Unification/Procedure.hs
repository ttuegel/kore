{-|
Module      : Kore.Unification.Procedure
Description : Unification procedure.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Unification.Procedure
    ( unificationProcedure
    ) where

import           Control.Applicative
                 ( empty )
import qualified Control.Monad.Trans.Class as Monad.Trans
import qualified Data.Text as Text
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Kore.Internal.MultiOr as MultiOr
                 ( extractPatterns )
import           Kore.Internal.Pattern
                 ( Conditional (..) )
import qualified Kore.Internal.Pattern as Conditional
import           Kore.Internal.Predicate
                 ( Predicate )
import           Kore.Internal.TermLike
import qualified Kore.Logger as Logger
import qualified Kore.Step.Merging.OrPattern as OrPattern
import           Kore.Step.Simplification.AndTerms
                 ( termUnification )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( makeEvaluateTerm )
import qualified Kore.Step.Simplification.Data as BranchT
                 ( scatter )
import           Kore.Step.Substitution
                 ( createPredicatesAndSubstitutionsMerger )
import           Kore.Syntax.Variable
                 ( SortedVariable )
import           Kore.Unification.Unify
                 ( MonadUnify )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

-- |'unificationProcedure' atempts to simplify @t1 = t2@, assuming @t1@ and @t2@
-- are terms (functional patterns) to a substitution.
-- If successful, it also produces a proof of how the substitution was obtained.
-- If failing, it gives a 'UnificationError' reason for the failure.
unificationProcedure
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        , FreshVariable variable
        , MonadUnify unifier
        , Logger.WithLog Logger.LogMessage unifier
        )
    => TermLike variable
    -> TermLike variable
    -> unifier (Predicate variable)
unificationProcedure  p1 p2
  | p1Sort /= p2Sort = do
    Monad.Unify.explainBottom
        "Cannot unify different sorts."
        p1
        p2
    empty
  | otherwise =
    Logger.withLogScope "UnificationProcedure" $ do
    prettyLogInfo $ Pretty.vsep
        [ "terms:"
        , Pretty.indent 4 $ unparse p1
        , Pretty.indent 2 "and"
        , Pretty.indent 4 $ unparse p2
        ]
    pat@Conditional { term } <- termUnification p1 p2
    prettyLogInfo $ Pretty.vsep
        [ "terms:"
        , Pretty.indent 4 $ unparse p1
        , Pretty.indent 2 "and"
        , Pretty.indent 4 $ unparse p2
        , "unifier:"
        , Pretty.indent 4 $ unparse pat
        ]
    if Conditional.isBottom pat
        then empty
        else Monad.Unify.liftBranchedSimplifier $ do
            orCeil <- Monad.Trans.lift $ Ceil.makeEvaluateTerm term
            orResult <-
                OrPattern.mergeWithPredicateAssumesEvaluated
                    createPredicatesAndSubstitutionsMerger
                    (Conditional.withoutTerm pat)
                    orCeil
            BranchT.scatter (MultiOr.extractPatterns orResult)
  where
    p1Sort = termLikeSort p1
    p2Sort = termLikeSort p2
    prettyLogInfo = Logger.logInfo . Text.pack . show
