{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Step.Rule.Expand
    ( ExpandSingleConstructors (..)
    ) where

import Prelude.Kore

import Data.List
    ( foldl'
    , foldr
    )
import Data.List.NonEmpty
    ( NonEmpty ((:|))
    )
import qualified Data.Map.Strict as Map

import qualified Debug
import Kore.Attribute.Pattern.FreeVariables
    ( freeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Attribute.Sort.Constructors as Attribute.Constructors
    ( Constructor (Constructor)
    , ConstructorLike (ConstructorLikeConstructor)
    , Constructors (Constructors)
    )
import qualified Kore.Attribute.Sort.Constructors as Constructors.DoNotUse
import Kore.IndexedModule.MetadataTools
    ( SmtMetadataTools
    , findSortConstructors
    )
import Kore.Internal.Predicate
    ( makeAndPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import qualified Kore.Internal.Substitution as Substitution
import Kore.Internal.TermLike
    ( TermLike
    , mkApplySymbol
    , mkElemVar
    )
import qualified Kore.Internal.TermLike as TermLike
    ( substitute
    )
import Kore.Sort
    ( Sort (..)
    , SortActual (SortActual)
    )
import qualified Kore.Sort as Sort.DoNotUse
import Kore.Step.RulePattern
    ( AllPathRule (..)
    , OnePathRule (..)
    , ReachabilityRule (..)
    , RulePattern (RulePattern)
    )
import qualified Kore.Step.RulePattern as RulePattern
import Kore.Syntax.ElementVariable
    ( ElementVariable (ElementVariable)
    )
import Kore.Syntax.Variable
    ( Variable (Variable, variableSort)
    )
import Kore.Variables.Fresh
    ( Avoiding
    , avoid
    , refreshVariable
    )
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (ElemVar)
    , extractElementVariable
    )
import qualified Pretty

-- | Instantiate variables on sorts with a single constructor
{- TODO(ttuegel): make this a strategy step, so that we expand any
    single-constructor variables at the start of each proof step.
    Going even further: make this a step in the variable simplifier?
-}
class ExpandSingleConstructors rule where
    expandSingleConstructors
        :: SmtMetadataTools attributes
        -> rule
        -> rule

instance ExpandSingleConstructors (RulePattern Variable) where
    expandSingleConstructors
        metadataTools
        rule@(RulePattern _ _ _ _ _)
      = case rule of
        RulePattern
            {left, antiLeft, requires
            , rhs = RulePattern.RHS {existentials, right, ensures}
            } ->
            let leftVariables :: [ElementVariable Variable]
                leftVariables =
                    mapMaybe extractElementVariable
                    $ FreeVariables.toList
                    $ freeVariables left
                allUnifiedVariables :: [UnifiedVariable Variable]
                allUnifiedVariables =
                    FreeVariables.toList (freeVariables rule)
                allElementVariables :: Avoiding (ElementVariable Variable)
                allElementVariables =
                    [ v | ElemVar v <- allUnifiedVariables]
                    & flip (++) existentials
                    & foldMap avoid
                expansion
                    :: Map.Map (UnifiedVariable Variable) (TermLike Variable)
                expansion =
                    expandVariables
                        metadataTools
                        leftVariables
                        allElementVariables
                substitutionPredicate =
                    ( Substitution.toPredicate
                    . Substitution.wrap
                    . Substitution.mkUnwrappedSubstitution
                    )
                        (Map.toList expansion)
            in rule
                { RulePattern.left = TermLike.substitute expansion left
                , RulePattern.antiLeft =
                    TermLike.substitute expansion <$> antiLeft
                , RulePattern.requires =
                    makeAndPredicate
                        (Predicate.substitute expansion requires)
                        substitutionPredicate
                , RulePattern.rhs = RulePattern.RHS
                    { existentials
                    , right = TermLike.substitute expansion right
                    , ensures = Predicate.substitute expansion ensures
                    }
                }

instance ExpandSingleConstructors OnePathRule where
    expandSingleConstructors tools =
        OnePathRule . expandSingleConstructors tools . getOnePathRule

instance ExpandSingleConstructors AllPathRule where
    expandSingleConstructors tools =
        AllPathRule . expandSingleConstructors tools . getAllPathRule

instance ExpandSingleConstructors ReachabilityRule where
    expandSingleConstructors tools (OnePath rule) =
        OnePath
        . OnePathRule
        . expandSingleConstructors tools
        . getOnePathRule
        $ rule
    expandSingleConstructors tools (AllPath rule) =
        AllPath
        . AllPathRule
        . expandSingleConstructors tools
        . getAllPathRule
        $ rule

expandVariables
    :: SmtMetadataTools attributes
    -> [ElementVariable Variable]
    -> Avoiding (ElementVariable Variable)
    -> Map.Map (UnifiedVariable Variable) (TermLike Variable)
expandVariables metadataTools variables toAvoid =
    fst $ foldl' expandAddVariable (Map.empty, toAvoid) variables
  where
    expandAddVariable
        ::  ( Map.Map (UnifiedVariable Variable) (TermLike Variable)
            , Avoiding (ElementVariable Variable)
            )
        -> ElementVariable Variable
        ->  ( Map.Map (UnifiedVariable Variable) (TermLike Variable)
            , Avoiding (ElementVariable Variable)
            )
    expandAddVariable (substitution, toAvoid') variable =
        case expandVariable metadataTools toAvoid' variable of
            (newVariables, term) ->
                ( if mkElemVar variable == term
                    then substitution
                    else Map.insert (ElemVar variable) term substitution
                , toAvoid' <> newVariables
                )

expandVariable
    :: SmtMetadataTools attributes
    -> Avoiding (ElementVariable Variable)
    -> ElementVariable Variable
    -> (Avoiding (ElementVariable Variable), TermLike Variable)
expandVariable
    metadataTools
    usedVariables
    variable@(ElementVariable Variable {variableSort})
  = expandSort metadataTools usedVariables variable UseDirectly variableSort

expandSort
    :: SmtMetadataTools attributes
    -> Avoiding (ElementVariable Variable)
    -> ElementVariable Variable
    -> VariableUsage
    -> Sort
    -> (Avoiding (ElementVariable Variable), TermLike Variable)
expandSort
    _metadataTools
    usedVariables
    defaultVariable
    variableUsage
    sort@(SortVariableSort _)
  =
    (updatedUsedVariables, variable)
  where
    (updatedUsedVariables, variable) =
        maybeNewVariable usedVariables defaultVariable sort variableUsage
expandSort
    metadataTools
    usedVariables
    defaultVariable
    variableUsage
    sort@(SortActualSort SortActual { sortActualName })
  =
    case findSortConstructors metadataTools sortActualName of
        Just
            (Attribute.Constructors.Constructors
                (Just
                    ( Attribute.Constructors.ConstructorLikeConstructor
                        constructor
                    :| []
                    )
                )
            ) ->
                expandConstructor
                    metadataTools
                    usedVariables
                    defaultVariable
                    constructor
        _ -> maybeNewVariable usedVariables defaultVariable sort variableUsage

expandConstructor
    :: SmtMetadataTools attributes
    -> Avoiding (ElementVariable Variable)
    -> ElementVariable Variable
    -> Attribute.Constructors.Constructor
    -> (Avoiding (ElementVariable Variable), TermLike Variable)
expandConstructor
    metadataTools
    usedVariables
    defaultVariable
    Attribute.Constructors.Constructor { name = symbol, sorts }
  = (finalUsedVariables, mkApplySymbol symbol children)
  where
    (children, finalUsedVariables) =
        foldr expandChildSort ([], usedVariables) sorts

    expandChildSort
        :: Sort
        -> ([TermLike Variable], Avoiding (ElementVariable Variable))
        -> ([TermLike Variable], Avoiding (ElementVariable Variable))
    expandChildSort sort (terms, beforeUsedVariables) =
        (term : terms, afterUsedVariables)
      where
        (afterUsedVariables, term) =
            expandSort
                metadataTools
                beforeUsedVariables
                defaultVariable
                UseAsPrototype
                sort

{-| Context: we have a TermLike that contains a variables, and we
attempt to expand them into constructor applications whenever that's possible.

We expand a variable by attempting to expand its sort into an unique
constructor application, and, recursively, the argument sorts of that
constructor.

This data type tells us how to use the initial variable that we are expanding
when we can't expand a sort, so we have to return a variable of that sort
instead.
-}
data VariableUsage =
    UseDirectly
    -- ^ We don't need to generate a new variable, we are at the top and
    -- we didn't manage to expand anything, so we can just reuse the
    -- variable in the original term as the sort's expansion (useful if we
    -- want to have prettier terms).
  | UseAsPrototype
    -- ^ We have expanded the initial sort at least once, so we need a variable
    -- somewhere in the middle of the expansion. We can't reuse the original
    -- variable, so we need to generate a new one based on it.

maybeNewVariable
    :: Avoiding (ElementVariable Variable)
    -> ElementVariable Variable
    -> Sort
    -> VariableUsage
    -> (Avoiding (ElementVariable Variable), TermLike Variable)
maybeNewVariable
    usedVariables
    variable@(ElementVariable Variable {variableSort})
    sort
    UseDirectly
  =
    if sort /= variableSort
        then error "Unmatching sort for direct use variable."
        else (usedVariables, mkElemVar variable)
maybeNewVariable usedVariables variable sort UseAsPrototype =
    case refreshVariable usedVariables variable' of
        Just newVariable ->
            ( avoid newVariable <> usedVariables
            , mkElemVar newVariable
            )
        Nothing ->
            (error . show . Pretty.hang 4 . Pretty.vsep)
                [ "Failed to generate a new name for:"
                , Pretty.indent 4 $ Debug.debug variable'
                , "while avoiding:"
                , Pretty.indent 4 $ Debug.debug usedVariables
                ]
  where
    variable' = resort variable
    resort (ElementVariable var) = ElementVariable var { variableSort = sort }
