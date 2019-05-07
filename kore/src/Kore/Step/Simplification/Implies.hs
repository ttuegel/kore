{-|
Module      : Kore.Step.Simplification.Implies
Description : Tools for Implies pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Implies
    ( simplify
    , simplifyEvaluated
    ) where

import qualified Kore.Attribute.Symbol as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import qualified Kore.Internal.MultiOr as MultiOr
import           Kore.Internal.OrPattern
                 ( OrPattern )
import qualified Kore.Internal.OrPattern as OrPattern
import           Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike
import qualified Kore.Predicate.Predicate as Syntax.Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Simplification.Data
                 ( PredicateSimplifier, Simplifier, TermLikeSimplifier )
import qualified Kore.Step.Simplification.Not as Not
                 ( makeEvaluate, simplifyEvaluated )
import           Kore.Syntax.Implies
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-|'simplify' simplifies an 'Implies' pattern with 'OrPattern'
children.

Right now this uses the following simplifications:

* a -> (b or c) = (a -> b) or (a -> c)
* bottom -> b = top
* top -> b = b
* a -> top = top
* a -> bottom = not a

and it has a special case for children with top terms.
-}
simplify
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Implies Sort (OrPattern variable)
    -> Simplifier (OrPattern variable)
simplify
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    Implies
        { impliesFirst = first
        , impliesSecond = second
        }
  =
    simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
        second

{-| simplifies an Implies given its two 'OrPattern' children.

See 'simplify' for details.
-}
-- TODO: Maybe transform this to (not a) \/ b
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Implies Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of two 'OrPattern' arguments. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually cache information
besides the pattern sort, which will make it even more useful to carry around.

-}
simplifyEvaluated
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> OrPattern variable
    -> OrPattern variable
    -> Simplifier (OrPattern variable)
simplifyEvaluated
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrPattern.isTrue first   = return second
  | OrPattern.isFalse first  = return OrPattern.top
  | OrPattern.isTrue second  = return OrPattern.top
  | OrPattern.isFalse second =
    Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
  | otherwise = do
    results <- traverse (simplifyEvaluateHalfImplies' first) second
    return (MultiOr.flatten results)
  where
    simplifyEvaluateHalfImplies' =
        simplifyEvaluateHalfImplies
            tools
            predicateSimplifier
            termSimplifier
            axiomSimplifiers

simplifyEvaluateHalfImplies
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> OrPattern variable
    -> Pattern variable
    -> Simplifier (OrPattern variable)
simplifyEvaluateHalfImplies
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    first
    second
  | OrPattern.isTrue first =
    return (OrPattern.fromPatterns [second])
  | OrPattern.isFalse first =
    return (OrPattern.fromPatterns [Pattern.top])
  | Pattern.isTop second =
    return (OrPattern.fromPatterns [Pattern.top])
  | Pattern.isBottom second =
    Not.simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        first
  | otherwise =
    -- TODO: Also merge predicate-only patterns for 'Or'
    return $ case MultiOr.extractPatterns first of
        [firstP] -> makeEvaluateImplies firstP second
        _ -> makeEvaluateImplies (OrPattern.toPattern first) second

makeEvaluateImplies
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => Pattern variable
    -> Pattern variable
    -> OrPattern variable
makeEvaluateImplies
    first second
  | Pattern.isTop first =
    OrPattern.fromPatterns [second]
  | Pattern.isBottom first =
    OrPattern.fromPatterns [Pattern.top]
  | Pattern.isTop second =
    OrPattern.fromPatterns [Pattern.top]
  | Pattern.isBottom second =
    Not.makeEvaluate first
  | otherwise =
    makeEvaluateImpliesNonBool first second

makeEvaluateImpliesNonBool
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => Pattern variable
    -> Pattern variable
    -> OrPattern variable
makeEvaluateImpliesNonBool
    pattern1@Conditional
        { term = firstTerm
        , predicate = firstPredicate
        , substitution = firstSubstitution
        }
    pattern2@Conditional
        { term = secondTerm
        , predicate = secondPredicate
        , substitution = secondSubstitution
        }
  | isTop firstTerm, isTop secondTerm =
    OrPattern.fromPatterns
        [ Conditional
            { term = firstTerm
            , predicate =
                Syntax.Predicate.makeImpliesPredicate
                    (Syntax.Predicate.makeAndPredicate
                        firstPredicate
                        (Syntax.Predicate.fromSubstitution firstSubstitution)
                    )
                    (Syntax.Predicate.makeAndPredicate
                        secondPredicate
                        (Syntax.Predicate.fromSubstitution secondSubstitution)
                    )
            , substitution = mempty
            }
        ]
  | otherwise =
    OrPattern.fromPatterns
        [ Conditional
            { term =
                mkImplies
                    (Pattern.toMLPattern pattern1)
                    (Pattern.toMLPattern pattern2)
            , predicate = Syntax.Predicate.makeTruePredicate
            , substitution = mempty
            }
        ]
