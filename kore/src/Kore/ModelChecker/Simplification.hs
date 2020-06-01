{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.ModelChecker.Simplification
    ( checkImplicationIsTop
    ) where

import Prelude.Kore

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Kore.Attribute.Pattern.FreeVariables
    ( freeVariables
    , getFreeElementVariables
    )
import Kore.Internal.Pattern
    ( Conditional (..)
    , Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
    ( ElementVariable
    , pattern Forall_
    , pattern Implies_
    , TermLike
    , mkAnd
    , mkCeil_
    , mkElemVar
    , mkNot
    )
import qualified Kore.Internal.TermLike as TermLike
import qualified Kore.Step.Simplification.Pattern as Pattern
    ( simplifyTopConfiguration
    )
import Kore.Step.Simplification.Simplify
import qualified Kore.Step.SMT.Evaluator as SMT.Evaluator
    ( filterMultiOr
    )
import Kore.TopBottom
    ( TopBottom (..)
    )
import Kore.Unparser
import Kore.Variables.Fresh
import qualified Pretty

checkImplicationIsTop
    :: MonadSimplify m
    => Pattern VariableName
    -> TermLike VariableName
    -> m Bool
checkImplicationIsTop lhs rhs =
    case stripForallQuantifiers rhs of
        ( forallQuantifiers, Implies_ _ implicationLHS implicationRHS ) -> do
            let rename = refreshVariables lhsFreeVariables forallQuantifiers
                subst = mkElemVar <$> Map.mapKeys inject rename
                implicationLHS' = TermLike.substitute subst implicationLHS
                implicationRHS' = TermLike.substitute subst implicationRHS
                resultTerm =
                    mkCeil_
                        (mkAnd
                            (mkAnd lhsMLPatt implicationLHS')
                            (mkNot implicationRHS')
                        )
                result = Conditional
                    { term = resultTerm
                    , predicate = Predicate.makeTruePredicate_
                    , substitution = mempty
                    }
            orResult <-
                Pattern.simplifyTopConfiguration result
            orFinalResult <- SMT.Evaluator.filterMultiOr orResult
            return (isBottom orFinalResult)
        _ -> (error . show . Pretty.vsep)
             [ "Not implemented error:"
             , "We don't know how to simplify the implication whose rhs is:"
             , Pretty.indent 4 (unparse rhs)
             ]
      where
        lhsFreeVariables =
            freeVariables lhs
            & getFreeElementVariables
            & map variableName
            & Set.fromList
        lhsMLPatt = Pattern.toTermLike lhs

stripForallQuantifiers
    :: TermLike VariableName
    -> (Set.Set (ElementVariable VariableName), TermLike VariableName)
stripForallQuantifiers patt
  = case patt of
        Forall_ _ forallVar child ->
            let
                ( childVars, strippedChild ) = stripForallQuantifiers child
            in
                ( Set.insert forallVar childVars, strippedChild)
        _ -> ( Set.empty , patt )
