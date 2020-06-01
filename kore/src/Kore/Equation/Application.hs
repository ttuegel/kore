{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Equation.Application
    ( attemptEquation
    , AttemptEquationResult
    , applyEquation
    -- * Errors
    , AttemptEquationError (..)
    , MatchError (..)
    , ApplyMatchResultErrors (..), ApplyMatchResultError (..)
    , CheckRequiresError (..)
    -- * Logging
    , DebugAttemptEquation (..)
    , DebugApplyEquation (..)
    , debugApplyEquation
    ) where

import Prelude.Kore

import Control.Error
    ( ExceptT
    , MaybeT (..)
    , noteT
    , runExceptT
    , throwE
    , withExceptT
    )
import Control.Monad
    ( (>=>)
    )
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty
    ( NonEmpty (..)
    )
import qualified Data.Map.Strict as Map
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Debug
import Kore.AST.AstWithLocation
import qualified Kore.Attribute.Axiom as Attribute
import Kore.Attribute.Pattern.FreeVariables
    ( HasFreeVariables (..)
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Attribute.Source as Attribute
import Kore.Equation.Equation
    ( Equation (..)
    )
import qualified Kore.Equation.Equation as Equation
import Kore.Internal.Condition
    ( Condition
    )
import qualified Kore.Internal.Condition as Condition
import qualified Kore.Internal.OrCondition as OrCondition
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( Predicate
    , makeAndPredicate
    , makeNotPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
import Kore.Internal.TermLike
    ( InternalVariable
    , TermLike
    )
import qualified Kore.Internal.TermLike as TermLike
import Kore.Step.Axiom.Matcher
    ( MatchResult
    , matchIncremental
    )
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    )
import qualified Kore.Step.Simplification.Simplify as Simplifier
import qualified Kore.Step.SMT.Evaluator as SMT
import Kore.Syntax.Id
    ( AstLocation (..)
    , FileLocation (..)
    )
import Kore.Syntax.Variable
import Kore.TopBottom
import Kore.Unparser
    ( Unparse (..)
    )
import Kore.Variables.Target
    ( Target
    )
import qualified Kore.Variables.Target as Target
import Log
    ( Entry (..)
    , MonadLog
    , Severity (..)
    , logEntry
    , logWhile
    )
import Pretty
    ( Pretty (..)
    )
import qualified Pretty

{- | The outcome of an attempt to apply an 'Equation'.

@AttemptEquationResult@ is 'Right' if the equation is applicable, and 'Left'
otherwise. If the equation is not applicable, the 'AttemptEquationError' will
indicate the reason.

 -}
type AttemptEquationResult variable =
    Either (AttemptEquationError variable) (Pattern variable)

{- | Attempt to apply an 'Equation' to the 'TermLike'.

The 'SideCondition' is used to evaluate the 'requires' clause of the 'Equation'.

The caller should use 'debugApplyEquation' to log when the result of an
equation is actually used; @attemptEquation@ will only log when an equation is
applicable.

 -}
attemptEquation
    :: forall simplifier variable
    .  MonadSimplify simplifier
    => InternalVariable variable
    => SideCondition (Target variable)
    -> TermLike (Target variable)
    -> Equation variable
    -> simplifier (AttemptEquationResult variable)
attemptEquation sideCondition termLike equation =
    whileDebugAttemptEquation' $ runExceptT $ do
        let Equation { left } = equationRenamed
        matchResult <- match left termLike & whileMatch
        (equation', predicate) <-
            applyMatchResult equationRenamed matchResult
            & whileApplyMatchResult
        let Equation { requires } = equation'
        checkRequires sideCondition predicate requires & whileCheckRequires
        let Equation { right, ensures } = equation'
        return $ Pattern.withCondition right $ from @(Predicate _) ensures
  where
    equationRenamed = targetEquationVariables sideCondition termLike equation
    matchError =
        MatchError
        { matchTerm = termLike
        , matchEquation = equationRenamed
        }
    match term1 term2 =
        matchIncremental term1 term2
        & MaybeT & noteT matchError

    whileDebugAttemptEquation'
        :: simplifier (AttemptEquationResult variable)
        -> simplifier (AttemptEquationResult variable)
    whileDebugAttemptEquation' action = do
        result <- whileDebugAttemptEquation termLike equationRenamed action
        debugAttemptEquationResult equation result
        return result

applyEquation
    :: forall simplifier variable
    .  MonadSimplify simplifier
    => InternalVariable variable
    => SideCondition variable
    -> Equation variable
    -> Pattern variable
    -> simplifier (OrPattern variable)
applyEquation _ equation result = do
    let results = OrPattern.fromPattern result
    let simplify = return
    debugApplyEquation equation result
    simplify results


{- | Use a 'MatchResult' to instantiate an 'Equation'.

The 'MatchResult' must cover all the free variables of the 'Equation'; this
condition is not checked, but enforced by the matcher. The result is the
'Equation' and any 'Predicate' assembled during matching, both instantiated by
the 'MatchResult'.

Throws 'ApplyMatchResultErrors' if there is a problem with the 'MatchResult'.

 -}
applyMatchResult
    :: forall monad variable
    .   Monad monad
    =>  InternalVariable variable
    =>  Equation (Target variable)
    ->  MatchResult (Target variable)
    ->  ExceptT (ApplyMatchResultErrors (Target variable)) monad
            (Equation variable, Predicate variable)
applyMatchResult equation matchResult@(predicate, substitution) = do
    case errors of
        x : xs ->
            throwE ApplyMatchResultErrors
                { matchResult
                , applyMatchErrors = x :| xs
                }
        _      -> return ()
    let predicate' =
            Predicate.substitute substitution predicate
            & Predicate.mapVariables (pure Target.unTarget)
        equation' =
            Equation.substitute substitution equation
            & Equation.mapVariables (pure Target.unTarget)
    return (equation', predicate')
  where
    equationVariables = freeVariables equation & FreeVariables.toList

    errors = concatMap checkVariable equationVariables

    checkVariable Variable { variableName } =
        case Map.lookup variableName substitution of
            Nothing -> [NotMatched variableName]
            Just termLike ->
                checkConcreteVariable variableName termLike
                <> checkSymbolicVariable variableName termLike

    checkConcreteVariable variable termLike
      | Set.member variable concretes
      , (not . TermLike.isConstructorLike) termLike
      = [NotConcrete variable termLike]
      | otherwise
      = empty

    checkSymbolicVariable variable termLike
      | Set.member variable symbolics
      , TermLike.isConstructorLike termLike
      = [NotSymbolic variable termLike]
      | otherwise
      = empty

    Equation { attributes } = equation
    concretes =
        attributes
        & Attribute.concrete
        & from @_ @(Set _)
    symbolics =
        attributes
        & Attribute.symbolic
        & from @_ @(Set _)

{- | Check that the requires from matching and the 'Equation' hold.

Throws 'RequiresNotMet' if the 'Predicate's do not hold under the
'SideCondition'.

 -}
checkRequires
    :: forall simplifier variable
    .  MonadSimplify simplifier
    => InternalVariable variable
    => SideCondition (Target variable)
    -> Predicate variable  -- ^ requires from matching
    -> Predicate variable  -- ^ requires from 'Equation'
    -> ExceptT (CheckRequiresError variable) simplifier ()
checkRequires sideCondition predicate requires =
    do
        let requires' = makeAndPredicate predicate requires
            -- The condition to refute:
            condition :: Condition variable
            condition = from @(Predicate _) (makeNotPredicate requires')
        return condition
            -- First try to refute 'condition' without user-defined axioms:
            >>= withoutAxioms . simplifyCondition
            -- Next try to refute 'condition' including user-defined axioms:
            >>= withAxioms . simplifyCondition
            -- Finally, try to refute the simplified 'condition' using the
            -- external solver:
            >>= SMT.filterBranch . withSideCondition
            >>= return . snd
    -- Collect the simplified results. If they are \bottom, then \and(predicate,
    -- requires) is valid; otherwise, the required pre-conditions are not met
    -- and the rule will not be applied.
    & (OrCondition.gather >=> assertBottom)
  where
    simplifyCondition = Simplifier.simplifyCondition sideCondition'

    -- TODO (thomas.tuegel): Do not unwrap sideCondition.
    sideCondition' =
        SideCondition.mapVariables
            (pure Target.unTarget)
            sideCondition

    assertBottom orCondition
      | isBottom orCondition = done
      | otherwise            = requiresNotMet
    done = return ()
    requiresNotMet =
        throwE CheckRequiresError
            { matchPredicate = predicate
            , equationRequires = requires
            }

    -- Pair a configuration with sideCondition for evaluation by the solver.
    withSideCondition = (,) sideCondition'

    withoutAxioms =
        fmap Condition.forgetSimplified
        . Simplifier.localSimplifierAxioms (const mempty)
    withAxioms = id

{- | Make the 'Equation' variables distinct from the initial pattern.

The variables are marked 'Target' and renamed to avoid any variables in the
'SideCondition' or the 'TermLike'.

 -}
targetEquationVariables
    :: forall variable
    .  InternalVariable variable
    => SideCondition (Target variable)
    -> TermLike (Target variable)
    -> Equation variable
    -> Equation (Target variable)
targetEquationVariables sideCondition initial =
    snd
    . Equation.refreshVariables avoiding
    . Equation.mapVariables Target.mkUnifiedTarget
  where
    avoiding = freeVariables sideCondition <> freeVariables initial

-- * Errors

{- | Errors that can occur during 'attemptEquation'.
 -}
data AttemptEquationError variable
    = WhileMatch !(MatchError (Target variable))
    | WhileApplyMatchResult !(ApplyMatchResultErrors (Target variable))
    | WhileCheckRequires !(CheckRequiresError variable)
    deriving (Show, Eq, Ord)
    deriving (GHC.Generic)

mapAttemptEquationErrorVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> AttemptEquationError variable1
    -> AttemptEquationError variable2
mapAttemptEquationErrorVariables adj =
    \case
        WhileMatch matchError ->
            WhileMatch $ mapMatchErrorVariables adjTarget matchError
        WhileApplyMatchResult applyMatchResultErrors ->
            WhileApplyMatchResult
            $ mapApplyMatchResultErrorsVariables
                adjTarget
                applyMatchResultErrors
        WhileCheckRequires checkRequiresError ->
            WhileCheckRequires
            $ mapCheckRequiresErrorVariables adj checkRequiresError
  where
    adjTarget = fmap <$> adj

whileMatch
    :: Functor monad
    => ExceptT (MatchError (Target variable)) monad a
    -> ExceptT (AttemptEquationError variable) monad a
whileMatch = withExceptT WhileMatch

whileApplyMatchResult
    :: Functor monad
    => ExceptT (ApplyMatchResultErrors (Target variable)) monad a
    -> ExceptT (AttemptEquationError variable) monad a
whileApplyMatchResult = withExceptT WhileApplyMatchResult

whileCheckRequires
    :: Functor monad
    => ExceptT (CheckRequiresError variable) monad a
    -> ExceptT (AttemptEquationError variable) monad a
whileCheckRequires = withExceptT WhileCheckRequires

instance SOP.Generic (AttemptEquationError variable)

instance SOP.HasDatatypeInfo (AttemptEquationError variable)

instance Debug variable => Debug (AttemptEquationError variable)

instance (Debug variable, Diff variable) => Diff (AttemptEquationError variable)

instance
    InternalVariable variable
    => Pretty (AttemptEquationError variable)
  where
    pretty (WhileMatch matchError) =
        pretty matchError
    pretty (WhileApplyMatchResult applyMatchResultErrors) =
        pretty applyMatchResultErrors
    pretty (WhileCheckRequires checkRequiresError) =
        pretty checkRequiresError

{- | Errors that can occur while matching the equation to the term.
 -}
data MatchError variable =
    MatchError
    { matchTerm :: !(TermLike variable)
    , matchEquation :: !(Equation variable)
    }
    deriving (Show, Eq, Ord)
    deriving (GHC.Generic)

instance SOP.Generic (MatchError variable)

instance SOP.HasDatatypeInfo (MatchError variable)

instance Debug variable => Debug (MatchError variable)

instance (Debug variable, Diff variable) => Diff (MatchError variable)

instance InternalVariable variable => Pretty (MatchError variable) where
    pretty _ = "equation did not match term"

mapMatchErrorVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> MatchError variable1
    -> MatchError variable2
mapMatchErrorVariables adj =
    \MatchError { matchTerm, matchEquation } ->
        MatchError
        { matchTerm = TermLike.mapVariables adj matchTerm
        , matchEquation = Equation.mapVariables adj matchEquation
        }

{- | Errors that can occur during 'applyMatchResult'.

There may be multiple independent reasons the match cannot be applied, so this
type contains a 'NonEmpty' list of 'ApplyMatchError'.

 -}
data ApplyMatchResultErrors variable =
    ApplyMatchResultErrors
    { matchResult :: !(MatchResult variable)
    , applyMatchErrors :: !(NonEmpty (ApplyMatchResultError variable))
    }
    deriving (Show, Eq, Ord)
    deriving (GHC.Generic)

instance SOP.Generic (ApplyMatchResultErrors variable)

instance SOP.HasDatatypeInfo (ApplyMatchResultErrors variable)

instance Debug variable => Debug (ApplyMatchResultErrors variable)

instance
    (Debug variable, Diff variable)
    => Diff (ApplyMatchResultErrors variable)

instance
    InternalVariable variable
    => Pretty (ApplyMatchResultErrors variable)
  where
    pretty ApplyMatchResultErrors { applyMatchErrors } =
        Pretty.vsep
        [ "could not apply match result:"
        , (Pretty.indent 4 . Pretty.vsep)
            (pretty <$> Foldable.toList applyMatchErrors)
        ]

mapApplyMatchResultErrorsVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> ApplyMatchResultErrors variable1
    -> ApplyMatchResultErrors variable2
mapApplyMatchResultErrorsVariables adj applyMatchResultErrors =
    ApplyMatchResultErrors
    { matchResult = mapMatchResultVariables adj matchResult
    , applyMatchErrors =
        mapApplyMatchResultErrorVariables adj <$> applyMatchErrors
    }
  where
    ApplyMatchResultErrors { matchResult, applyMatchErrors } =
        applyMatchResultErrors

mapMatchResultVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> MatchResult variable1
    -> MatchResult variable2
mapMatchResultVariables adj (predicate, substitution) =
    ( Predicate.mapVariables adj predicate
    , mapSubstitutionVariables substitution
    )
  where
    mapSubstitutionVariables =
       Map.mapKeys (mapSomeVariableName adj)
       . Map.map (TermLike.mapVariables adj)

{- | @ApplyMatchResultError@ represents a reason the match could not be applied.
 -}
data ApplyMatchResultError variable
    = NotConcrete (SomeVariableName variable) (TermLike variable)
    -- ^ The variable was matched with a symbolic term where a concrete
    -- term was required.
    | NotSymbolic (SomeVariableName variable) (TermLike variable)
    -- ^ The variable was matched with a concrete term where a symbolic
    -- term was required.
    | NotMatched (SomeVariableName variable)
    -- ^ The variable was not matched.
    deriving (Show, Eq, Ord)
    deriving (GHC.Generic)

instance SOP.Generic (ApplyMatchResultError variable)

instance SOP.HasDatatypeInfo (ApplyMatchResultError variable)

instance Debug variable => Debug (ApplyMatchResultError variable)

instance
    (Debug variable, Diff variable)
    => Diff (ApplyMatchResultError variable)

instance
    InternalVariable variable
    => Pretty (ApplyMatchResultError variable)
  where
    pretty (NotConcrete variable _) =
        Pretty.hsep
        [ "variable"
        , unparse variable
        , "did not match a concrete term"
        ]
    pretty (NotSymbolic variable _) =
        Pretty.hsep
        [ "variable"
        , unparse variable
        , "did not match a symbolic term"
        ]
    pretty (NotMatched variable) =
        Pretty.hsep ["variable", unparse variable, "was not matched"]

mapApplyMatchResultErrorVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> ApplyMatchResultError variable1
    -> ApplyMatchResultError variable2
mapApplyMatchResultErrorVariables adj applyMatchResultError =
    case applyMatchResultError of
        NotConcrete variable termLike ->
            NotConcrete
                (mapSomeVariableName' variable)
                (mapTermLikeVariables termLike)
        NotSymbolic variable termLike ->
            NotSymbolic
                (mapSomeVariableName' variable)
                (mapTermLikeVariables termLike)
        NotMatched variable -> NotMatched (mapSomeVariableName' variable)
  where
    mapSomeVariableName' = mapSomeVariableName adj
    mapTermLikeVariables = TermLike.mapVariables adj

{- | Errors that can occur during 'checkRequires'.
 -}
data CheckRequiresError variable =
    CheckRequiresError
    { matchPredicate :: !(Predicate variable)
    , equationRequires :: !(Predicate variable)
    }
    deriving (Show, Eq, Ord)
    deriving (GHC.Generic)

instance SOP.Generic (CheckRequiresError variable)

instance SOP.HasDatatypeInfo (CheckRequiresError variable)

instance Debug variable => Debug (CheckRequiresError variable)

instance (Debug variable, Diff variable) => Diff (CheckRequiresError variable)

instance InternalVariable variable => Pretty (CheckRequiresError variable) where
    pretty CheckRequiresError { matchPredicate, equationRequires } =
        Pretty.vsep
        [ "could not infer the equation requirement:"
        , Pretty.indent 4 (unparse equationRequires)
        , "and the matching requirement:"
        , Pretty.indent 4 (unparse matchPredicate)
        ]

mapCheckRequiresErrorVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> CheckRequiresError variable1
    -> CheckRequiresError variable2
mapCheckRequiresErrorVariables adj checkRequiresError =
    CheckRequiresError
    { matchPredicate = mapPredicateVariables matchPredicate
    , equationRequires = mapPredicateVariables equationRequires
    }
  where
    mapPredicateVariables = Predicate.mapVariables adj
    CheckRequiresError { matchPredicate, equationRequires } = checkRequiresError

-- * Logging

{- | Log entries for all phases of equation application.
 -}
data DebugAttemptEquation
    = DebugAttemptEquation (Equation VariableName) (TermLike VariableName)
    -- ^ Covers the entire scope of 'attemptEquation'.
    | DebugAttemptEquationResult
        (Equation VariableName)
        (AttemptEquationResult VariableName)
    -- ^ Entered into the log when an equation is applicable.
    deriving (Show)
    deriving (GHC.Generic)

instance Pretty DebugAttemptEquation where
    pretty (DebugAttemptEquation equation termLike) =
        Pretty.vsep
        [ (Pretty.hsep . catMaybes)
            [ Just "applying equation"
            , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
            , Just "to term:"
            ]
        , Pretty.indent 4 (unparse termLike)
        ]
    pretty (DebugAttemptEquationResult _ (Left attemptEquationError)) =
        Pretty.vsep
        [ "equation is not applicable:"
        , pretty attemptEquationError
        ]
    pretty (DebugAttemptEquationResult _ (Right _)) =
        "equation is applicable"

instance Entry DebugAttemptEquation where
    entrySeverity _ = Debug
    shortDoc (DebugAttemptEquation equation _) =
        (Just . Pretty.hsep . catMaybes)
            [ Just "while applying equation"
            , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
            ]
    shortDoc _ = Nothing
    helpDoc _ = "log equation application attempts"

{- | Log the result of attempting to apply an 'Equation'.

 -}
debugAttemptEquationResult
    :: MonadLog log
    => InternalVariable variable
    => Equation variable
    -> AttemptEquationResult variable
    -> log ()
debugAttemptEquationResult equation result =
    logEntry $ DebugAttemptEquationResult
        (Equation.mapVariables (pure toVariableName) equation)
        (mapAttemptEquationResultVariables (pure toVariableName) result)

mapAttemptEquationResultVariables
    :: (InternalVariable variable1, InternalVariable variable2)
    => AdjSomeVariableName (variable1 -> variable2)
    -> AttemptEquationResult variable1
    -> AttemptEquationResult variable2
mapAttemptEquationResultVariables adj =
    Bifunctor.bimap
        (mapAttemptEquationErrorVariables adj)
        (Pattern.mapVariables adj)

whileDebugAttemptEquation
    :: MonadLog log
    => InternalVariable variable
    => TermLike variable
    -> Equation variable
    -> log a
    -> log a
whileDebugAttemptEquation termLike equation =
    logWhile (DebugAttemptEquation equation' termLike')
  where
    termLike' = TermLike.mapVariables (pure toVariableName) termLike
    equation' = Equation.mapVariables (pure toVariableName) equation

{- | Log when an 'Equation' is actually applied.
 -}
data DebugApplyEquation
    = DebugApplyEquation (Equation VariableName) (Pattern VariableName)
    -- ^ Entered into the log when an equation's result is actually used.
    deriving (Show)
    deriving (GHC.Generic)

instance Pretty DebugApplyEquation where
    pretty (DebugApplyEquation equation result) =
        Pretty.vsep
        [ (Pretty.hsep . catMaybes)
            [ Just "applied equation"
            , (\loc -> Pretty.hsep ["at", pretty loc]) <$> srcLoc equation
            , Just "with result:"
            ]
        , Pretty.indent 4 (unparse result)
        ]

srcLoc :: Equation VariableName -> Maybe Attribute.SourceLocation
srcLoc equation
  | (not . isLocEmpty) kLoc = Just kLoc
  | AstLocationFile fileLocation <- locationFromAst equation =
    Just (from @FileLocation fileLocation)
  | otherwise = Nothing
  where
    kLoc = Attribute.sourceLocation $ attributes equation

isLocEmpty :: Attribute.SourceLocation -> Bool
isLocEmpty Attribute.SourceLocation { source = Attribute.Source file } =
    isNothing file

instance Entry DebugApplyEquation where
    entrySeverity _ = Debug
    helpDoc _ = "log equation application successes"

{- | Log when an 'Equation' is actually applied.

@debugApplyEquation@ is different from 'debugAttemptEquationResult', which
only indicates if an equation is applicable, that is: if it could apply. If
multiple equations are applicable in the same place, the caller will determine
which is actually applied. Therefore, the /caller/ should use this log entry
after 'attemptEquation'.

 -}
debugApplyEquation
    :: MonadLog log
    => InternalVariable variable
    => Equation variable
    -> Pattern variable
    -> log ()
debugApplyEquation equation result =
    logEntry $ DebugApplyEquation equation' result'
  where
    equation' = Equation.mapVariables (pure toVariableName) equation
    result' = Pattern.mapVariables (pure toVariableName) result
