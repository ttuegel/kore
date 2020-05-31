{-|
Module      : Kore.Step.Axiom.EvaluationStrategy
Description : Various strategies for axiom/builtin-based simplification.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Axiom.EvaluationStrategy
    ( builtinEvaluation
    , definitionEvaluation
    , simplificationEvaluation
    , firstFullEvaluation
    , simplifierWithFallback
    ) where

import Control.Category
    ( (>>>)
    )
import Prelude.Kore

import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.Semigroup
    ( Min (..)
    , Option (..)
    )
import qualified Data.Text as Text

import qualified Kore.Attribute.Symbol as Attribute
import Kore.Equation
    ( Equation
    )
import qualified Kore.Equation as Equation
import qualified Kore.Internal.MultiOr as MultiOr
    ( extractPatterns
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
import Kore.Internal.Symbol
import Kore.Internal.TermLike as TermLike
import Kore.Step.Simplification.Simplify
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiom
    ( AttemptedAxiom (..)
    )
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiomResults
    ( AttemptedAxiomResults (..)
    )
import Kore.Unparser
    ( unparse
    )
import qualified Kore.Variables.Target as Target
import qualified Pretty

{-|Describes whether simplifiers are allowed to return multiple results or not.
-}
data AcceptsMultipleResults = WithMultipleResults | OnlyOneResult
    deriving (Eq, Ord, Show)

{-|Converts 'AcceptsMultipleResults' to Bool.
-}
acceptsMultipleResults :: AcceptsMultipleResults -> Bool
acceptsMultipleResults WithMultipleResults = True
acceptsMultipleResults OnlyOneResult = False

{-| Creates an evaluator for a function from the full set of rules
that define it.
-}
definitionEvaluation
    :: [Equation VariableName]
    -> BuiltinAndAxiomSimplifier
definitionEvaluation equations =
    BuiltinAndAxiomSimplifier $ \term condition -> do
        let equations' =
                Equation.mapVariables (pure fromVariableName)
                <$> equations
            term' = TermLike.mapVariables Target.mkUnifiedNonTarget term
            condition' =
                SideCondition.mapVariables
                    Target.mkUnifiedNonTarget
                    condition
        let -- Attempt an equation, pairing it with its result, if applicable.
            attemptEquation equation =
                Equation.attemptEquation condition' term' equation
                >>= return . Bifunctor.second apply
              where
                apply = Equation.applyEquation condition equation
        traverse attemptEquation equations' >>=
            (partitionEithers >>> \case
                (_, applied : _) -> do
                    results <- applied
                    (return . Applied) AttemptedAxiomResults
                        { results
                        , remainders = OrPattern.bottom
                        }
                (errors, []) ->
                    case minError of
                        Just (Equation.WhileCheckRequires _) ->
                            (return . NotApplicableUntilConditionChanges)
                                (SideCondition.toRepresentation condition)
                        _ -> return NotApplicable
                  where
                    minError =
                        (fmap getMin . getOption)
                        (foldMap (Option . Just . Min) errors)
            )

-- | Create an evaluator from a single simplification rule.
simplificationEvaluation
    :: Equation VariableName
    -> BuiltinAndAxiomSimplifier
simplificationEvaluation equation =
    BuiltinAndAxiomSimplifier $ \term condition -> do
        let equation' = Equation.mapVariables (pure fromVariableName) equation
            term' = TermLike.mapVariables Target.mkUnifiedNonTarget term
            condition' =
                SideCondition.mapVariables
                    Target.mkUnifiedNonTarget
                    condition
        result <- Equation.attemptEquation condition' term' equation'
        let apply = Equation.applyEquation condition equation'
        case result of
            Right applied -> do
                results <- apply applied
                (return . Applied) AttemptedAxiomResults
                    { results
                    , remainders = OrPattern.bottom
                    }
            Left err ->
                case err of
                    Equation.WhileCheckRequires _ ->
                        (return . NotApplicableUntilConditionChanges)
                            (SideCondition.toRepresentation condition)
                    _ -> return NotApplicable

{-| Creates an evaluator that choses the result of the first evaluator that
returns Applicable.

If that result contains more than one pattern, or it contains a reminder,
the evaluation fails with 'error' (may change in the future).
-}
firstFullEvaluation
    :: [BuiltinAndAxiomSimplifier]
    -> BuiltinAndAxiomSimplifier
firstFullEvaluation simplifiers =
    BuiltinAndAxiomSimplifier
        (applyFirstSimplifierThatWorks simplifiers OnlyOneResult)

{-| Creates an evaluator that choses the result of the first evaluator if it
returns Applicable, otherwise returns the result of the second.
-}
simplifierWithFallback
    :: BuiltinAndAxiomSimplifier
    -> BuiltinAndAxiomSimplifier
    -> BuiltinAndAxiomSimplifier
simplifierWithFallback first second =
    BuiltinAndAxiomSimplifier
        (applyFirstSimplifierThatWorks [first, second] WithMultipleResults)

{-| Wraps an evaluator for builtins. Will fail with error if there is no result
on concrete patterns.
-}
builtinEvaluation
    :: BuiltinAndAxiomSimplifier
    -> BuiltinAndAxiomSimplifier
builtinEvaluation evaluator =
    BuiltinAndAxiomSimplifier (evaluateBuiltin evaluator)

evaluateBuiltin
    :: forall variable simplifier
    .  ( InternalVariable variable
       , MonadSimplify simplifier
       )
    => BuiltinAndAxiomSimplifier
    -- ^ Map from axiom IDs to axiom evaluators
    -> TermLike variable
    -> SideCondition variable
    -> simplifier (AttemptedAxiom variable)
evaluateBuiltin
    (BuiltinAndAxiomSimplifier builtinEvaluator)
    patt
    sideCondition
  = do
    result <- builtinEvaluator patt sideCondition
    case result of
        AttemptedAxiom.NotApplicable
          | App_ appHead children <- patt
          , Just hook_ <- Text.unpack <$> Attribute.getHook (symbolHook appHead)
          , all isValue children ->
            (error . show . Pretty.vsep)
                [ "Expecting hook "
                    <> Pretty.squotes (Pretty.pretty hook_)
                    <> " to reduce concrete pattern:"
                , Pretty.indent 4 (unparse patt)
                ]
        _ -> return result
  where
    isValue pat =
        maybe False TermLike.isConstructorLike $ asConcrete pat

{-|Whether a term cannot be simplified regardless of the side condition,
or only with the current side condition.

Example usage for @applyFirstSimplifierThatWorksWorker@:

We start assuming that if we can't simplify the current term, we will never
be able to simplify it.

If we manage to apply one of the evaluators with an acceptable result
(e.g. without remainders), we just return the result and we ignore the
value of the @NonSimplifiability@ argument.

If the result is not acceptable, we continue trying other evaluators, but we
assume that, even if we are not able to simplify the term right now, that may
change when the current side condition changes (i.e. we send @Conditional@
as an argument to the next @applyFirstSimplifierThatWorksWorker@ call).

If we finished trying all the evaluators without an acceptable result,
we mark the term as simplified according to the 'NonSimplifiability' argument,
either as "always simplified", or as "simplified while the current side
condition is unchanged".
-}
data NonSimplifiability
    = Always
    | Conditional

applyFirstSimplifierThatWorks
    :: forall variable simplifier
    .  ( InternalVariable variable
       , MonadSimplify simplifier
       )
    => [BuiltinAndAxiomSimplifier]
    -> AcceptsMultipleResults
    -> TermLike variable
    -> SideCondition variable
    -> simplifier (AttemptedAxiom variable)
applyFirstSimplifierThatWorks evaluators multipleResults =
    applyFirstSimplifierThatWorksWorker evaluators multipleResults Always

applyFirstSimplifierThatWorksWorker
    :: forall variable simplifier
    .  ( InternalVariable variable
       , MonadSimplify simplifier
       )
    => [BuiltinAndAxiomSimplifier]
    -> AcceptsMultipleResults
    -> NonSimplifiability
    -> TermLike variable
    -> SideCondition variable
    -> simplifier (AttemptedAxiom variable)
applyFirstSimplifierThatWorksWorker [] _ Always _ _ =
    return AttemptedAxiom.NotApplicable
applyFirstSimplifierThatWorksWorker [] _ Conditional _ sideCondition =
    return
    $ AttemptedAxiom.NotApplicableUntilConditionChanges
    $ SideCondition.toRepresentation sideCondition
applyFirstSimplifierThatWorksWorker
    (BuiltinAndAxiomSimplifier evaluator : evaluators)
    multipleResults
    nonSimplifiability
    patt
    sideCondition
  = do
    applicationResult <- evaluator patt sideCondition

    case applicationResult of
        AttemptedAxiom.Applied AttemptedAxiomResults
            { results = orResults
            , remainders = orRemainders
            }
          | acceptsMultipleResults multipleResults -> return applicationResult
          -- below this point multiple results are not accepted
          | length (MultiOr.extractPatterns orResults) > 1 ->
            -- We should only allow multiple simplification results
            -- when they are created by unification splitting the
            -- configuration.
            -- However, right now, we shouldn't be able to get more
            -- than one result, so we throw an error.
            error . show . Pretty.vsep $
                [ "Unexpected simplification result with more \
                    \than one configuration:"
                , Pretty.indent 2 "input:"
                , Pretty.indent 4 (unparse patt)
                , Pretty.indent 2 "results:"
                , (Pretty.indent 4 . Pretty.vsep)
                    (unparse <$> Foldable.toList orResults)
                , Pretty.indent 2 "remainders:"
                , (Pretty.indent 4 . Pretty.vsep)
                    (unparse <$> Foldable.toList orRemainders)
                ]
          | not (OrPattern.isFalse orRemainders) ->
            tryNextSimplifier Conditional
          | otherwise -> return applicationResult
        AttemptedAxiom.NotApplicable -> tryNextSimplifier nonSimplifiability
        AttemptedAxiom.NotApplicableUntilConditionChanges _ ->
            tryNextSimplifier Conditional
  where
    tryNextSimplifier nonSimplifiability' =
        applyFirstSimplifierThatWorksWorker
            evaluators
            multipleResults
            nonSimplifiability'
            patt
            sideCondition
