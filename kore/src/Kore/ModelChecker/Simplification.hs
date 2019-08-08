{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.ModelChecker.Simplification
    ( checkImplicationIsTop
    ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import           Kore.Internal.Pattern
                 ( Conditional (..), Pattern )
import qualified Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike as TermLike
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.OrPattern as OrPattern
                 ( filterMultiOrWithTermCeil )
import qualified Kore.Step.Simplification.Pattern as Pattern
                 ( simplifyAndRemoveTopExists )
import           Kore.TopBottom
                 ( TopBottom (..) )
import           Kore.Unparser
import           Kore.Variables.Fresh
import           Kore.Variables.UnifiedVariable
                 ( UnifiedVariable (..) )

checkImplicationIsTop
    :: MonadSimplify m
    => Pattern Variable
    -> TermLike Variable
    -> m Bool
checkImplicationIsTop lhs rhs =
    case stripForallQuantifiers rhs of
        ( forallQuantifiers, Implies_ _ implicationLHS implicationRHS ) -> do
            let rename = refreshVariables lhsFreeVariables forallQuantifiers
                subst = mkElemVar <$> Map.mapKeys ElemVar rename
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
                    , predicate = Predicate.makeTruePredicate
                    , substitution = mempty
                    }
            orResult <- Pattern.simplifyAndRemoveTopExists result
            orFinalResult <- OrPattern.filterMultiOrWithTermCeil orResult
            return (isBottom orFinalResult)
        _ -> (error . show . Pretty.vsep)
             [ "Not implemented error:"
             , "We don't know how to simplify the implication whose rhs is:"
             , Pretty.indent 4 (unparse rhs)
             ]
      where
        lhsFreeVariables = Set.fromList $
            FreeVariables.getFreeElementVariables (Pattern.freeVariables lhs)
        lhsMLPatt = Pattern.toTermLike lhs

stripForallQuantifiers
    :: TermLike Variable
    -> (Set.Set (ElementVariable Variable), TermLike Variable)
stripForallQuantifiers patt
  = case patt of
        Forall_ _ forallVar child ->
            let
                ( childVars, strippedChild ) = stripForallQuantifiers child
            in
                ( Set.insert forallVar childVars, strippedChild)
        _ -> ( Set.empty , patt )
