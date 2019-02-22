{- |
Module      : Kore.AST.Valid
Description : Constructors and patterns for valid Kore syntax trees
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module implements an interface analogous to
"Kore.ASTUtils.SmartConstructors" and "Kore.ASTUtils.SmartPatterns" for 'Valid'
patterns. Unlike the @SmartConstructors@, this module does not require
"Kore.IndexedModule.MetadataTools".

 -}

module Kore.AST.Valid
    (
    -- * Utility functions for dealing with sorts
      getSort
    , getSortAttributes
    , forceSort
    , predicateSort
    , localInPattern
    , inPath
    , isObviouslyPredicate
    -- * Pure Kore pattern constructors
    , mkAnd
    , mkApp
    , mkBottom
    , mkCeil
    , mkDomainValue
    , mkEquals
    , mkExists
    , mkFloor
    , mkForall
    , mkIff
    , mkImplies
    , mkIn
    , mkNext
    , mkNot
    , mkOr
    , mkRewrites
    , mkTop
    , mkVar
    , mkStringLiteral
    , mkCharLiteral
    , mkSort
    , mkSortVariable
    , varS
    , symS
    -- * Predicate constructors
    , mkBottom_
    , mkCeil_
    , mkEquals_
    , mkFloor_
    , mkIn_
    , mkTop_
    -- * Sentence constructors
    , mkAlias
    , mkAlias_
    , mkAxiom
    , mkAxiom_
    , mkSymbol
    , mkSymbol_
    -- * Application constructors
    , applyAlias
    , applyAlias_
    , applySymbol
    , applySymbol_
    -- * Pattern synonyms
    , pattern And_
    , pattern App_
    , pattern Bottom_
    , pattern Ceil_
    , pattern DV_
    , pattern Equals_
    , pattern Exists_
    , pattern Floor_
    , pattern Forall_
    , pattern Iff_
    , pattern Implies_
    , pattern In_
    , pattern Next_
    , pattern Not_
    , pattern Or_
    , pattern Rewrites_
    , pattern Top_
    , pattern Var_
    , pattern V
    , pattern StringLiteral_
    , pattern CharLiteral_
    -- * Re-exports
    , module Valid
    ) where

import           Control.Applicative
import           Control.Comonad
import           Data.Align
import qualified Data.Default as Default
import           Data.Foldable
import qualified Data.Functor.Foldable as Recursive
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import           Data.These

import           Kore.Annotation.Valid as Valid
import           Kore.AST.Lens
import           Kore.AST.Pure
import           Kore.AST.Sentence
import qualified Kore.Attribute.Sort as Attribute
import           Kore.Implicit.ImplicitSorts
import           Kore.Unparser
                 ( Unparse, renderDefault, unparseToString )
import qualified Kore.Unparser as Unparse

-- | Get the 'Sort' of a 'PurePattern' from the 'Valid' annotation.
getSort
    :: Functor domain
    => PurePattern level domain variable (Valid (variable level) level)
    -> Sort level
getSort (extract -> Valid { patternSort }) = patternSort

getSortAttributes
    :: Functor domain
    => PurePattern level domain variable (Valid (variable level) level)
    -> Maybe Attribute.Sort
getSortAttributes purePattern =
    case getSort purePattern of
        SortActualSort SortActual { sortAttributes } -> Just sortAttributes
        _ -> Nothing

-- | Attempts to modify p to have sort s.
forceSort
    ::  ( Traversable domain
        , Unparse pattern'
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => Sort level
    -> pattern'
    -> pattern'
forceSort forcedSort = Recursive.apo forceSortWorker
  where
    forceSortWorker original@(Recursive.project -> valid :< pattern') =
        (valid { patternSort = forcedSort } :<)
            (case valid of
                Valid { patternSort }
                  | patternSort == forcedSort    -> Left <$> pattern'
                  | patternSort == predicateSort -> forceSortWorkerPredicate
                  | otherwise                    -> illSorted
            )
      where
        illSorted =
            (error . unlines)
            [ "Could not force pattern to sort '"
                ++ unparseToString forcedSort ++ "':"
            , unparseToString original
            ]
        forceSortWorkerPredicate =
            case pattern' of
                -- Predicates: Force sort and stop.
                BottomPattern bottom' ->
                    BottomPattern bottom' { bottomSort = forcedSort }
                TopPattern top' ->
                    TopPattern top' { topSort = forcedSort }
                CeilPattern ceil' ->
                    CeilPattern (Left <$> ceil'')
                  where
                    ceil'' = ceil' { ceilResultSort = forcedSort }
                FloorPattern floor' ->
                    FloorPattern (Left <$> floor'')
                  where
                    floor'' = floor' { floorResultSort = forcedSort }
                EqualsPattern equals' ->
                    EqualsPattern (Left <$> equals'')
                  where
                    equals'' = equals' { equalsResultSort = forcedSort }
                InPattern in' ->
                    InPattern (Left <$> in'')
                  where
                    in'' = in' { inResultSort = forcedSort }
                -- Connectives: Force sort and recurse.
                AndPattern and' ->
                    AndPattern (Right <$> and'')
                  where
                    and'' = and' { andSort = forcedSort }
                OrPattern or' ->
                    OrPattern (Right <$> or'')
                  where
                    or'' = or' { orSort = forcedSort }
                IffPattern iff' ->
                    IffPattern (Right <$> iff'')
                  where
                    iff'' = iff' { iffSort = forcedSort }
                ImpliesPattern implies' ->
                    ImpliesPattern (Right <$> implies'')
                  where
                    implies'' = implies' { impliesSort = forcedSort }
                NotPattern not' ->
                    NotPattern (Right <$> not'')
                  where
                    not'' = not' { notSort = forcedSort }
                NextPattern next' ->
                    NextPattern (Right <$> next'')
                  where
                    next'' = next' { nextSort = forcedSort }
                RewritesPattern rewrites' ->
                    RewritesPattern (Right <$> rewrites'')
                  where
                    rewrites'' = rewrites' { rewritesSort = forcedSort }
                ExistsPattern exists' ->
                    ExistsPattern (Right <$> exists'')
                  where
                    exists'' = exists' { existsSort = forcedSort }
                ForallPattern forall' ->
                    ForallPattern (Right <$> forall'')
                  where
                    forall'' = forall' { forallSort = forcedSort }
                -- Rigid: These patterns should never have sort _PREDICATE{}.
                ApplicationPattern _ -> illSorted
                DomainValuePattern _ -> illSorted
                CharLiteralPattern _ -> illSorted
                StringLiteralPattern _ -> illSorted
                VariablePattern _ -> illSorted

{- | Call the argument function with two patterns whose sorts agree.

If one pattern is flexibly sorted, the result is the rigid sort of the other
pattern. If both patterns are flexibly sorted, then the result is
'predicateSort'. If both patterns have the same rigid sort, that is the
result. It is an error if the patterns are rigidly sorted but do not have the
same sort.

 -}
makeSortsAgree
    ::  ( Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => (pattern' -> pattern' -> Sort level -> a)
    -> pattern'
    -> pattern'
    -> a
makeSortsAgree withPatterns = \pattern1 pattern2 ->
    let
        sort1 = getRigidSort pattern1
        sort2 = getRigidSort pattern2
        sort = fromMaybe predicateSort (sort1 <|> sort2)
        !pattern1' = forceSort sort pattern1
        !pattern2' = forceSort sort pattern2
    in
        withPatterns pattern1' pattern2' sort
{-# INLINE makeSortsAgree #-}

getRigidSort
    ::  ( Traversable domain
        , valid ~ Valid (variable level) level
        )
    => PurePattern level domain variable valid
    -> Maybe (Sort level)
getRigidSort pattern' =
    case getSort pattern' of
        sort
          | sort == predicateSort -> Nothing
          | otherwise -> Just sort

{- | Construct an 'And' pattern.
 -}
mkAnd
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkAnd = makeSortsAgree mkAndWorker
  where
    mkAndWorker andFirst andSecond andSort =
        asPurePattern (valid :< AndPattern and')
      where
        valid = Valid { patternSort = andSort, freeVariables }
        and' = And { andSort, andFirst, andSecond }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract andFirst
            Valid { freeVariables = freeVariables2 } = extract andSecond

{- | Construct an 'Application' pattern.

The result sort of the 'SymbolOrAlias' must be provided. The sorts of arguments
are not checked. Use 'applySymbol' or 'applyAlias' whenever possible to avoid
these shortcomings.

See also: 'applyAlias', 'applySymbol'

 -}
-- TODO: Should this check for sort agreement?
mkApp
    ::  ( Functor domain
        , Ord (variable level)
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => Sort level
    -- ^ Result sort
    -> SymbolOrAlias level
    -- ^ Application symbol or alias
    -> [pattern']
    -- ^ Application arguments
    -> pattern'
mkApp patternSort applicationSymbolOrAlias applicationChildren =
    asPurePattern (valid :< ApplicationPattern application)
  where
    valid = Valid { patternSort, freeVariables }
    application = Application { applicationSymbolOrAlias, applicationChildren }
    freeVariables =
        Set.unions (Valid.freeVariables . extract <$> applicationChildren)

{- | The 'Sort' substitution from applying the given sort parameters.
 -}
sortSubstitution
    :: [SortVariable level]
    -> [Sort level]
    -> Map.Map (SortVariable level) (Sort level)
sortSubstitution variables sorts =
    foldl' insertSortVariable Map.empty (align variables sorts)
  where
    insertSortVariable map' =
        \case
            These var sort -> Map.insert var sort map'
            This _ -> (error . unlines) ("Too few parameters." : expected)
            That _ -> (error . unlines) ("Too many parameters." : expected)
    expected =
        [ "Expected:"
        , renderDefault (Unparse.parameters variables)
        , "but found:"
        , renderDefault (Unparse.parameters sorts)
        ]

{- | Construct an 'Application' pattern from a 'Alias' declaration.

The provided sort parameters must match the declaration.

See also: 'mkApp', 'applyAlias_', 'applySymbol', 'mkAlias'

 -}
applyAlias
    ::  ( Traversable domain
        , Ord (variable level)
        , Unparse pattern'
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => SentenceAlias level pattern'
    -- ^ 'Alias' declaration
    -> [Sort level]
    -- ^ 'Alias' sort parameters
    -> [pattern']
    -- ^ 'Application' arguments
    -> pattern'
applyAlias sentence params children =
    mkApp
        resultSort'
        symbolOrAlias
        children'
  where
    SentenceAlias { sentenceAliasAlias = alias } = sentence
    SentenceAlias { sentenceAliasResultSort } = sentence
    Alias { aliasConstructor } = alias
    Alias { aliasParams } = alias
    symbolOrAlias =
        SymbolOrAlias
            { symbolOrAliasConstructor = aliasConstructor
            , symbolOrAliasParams = params
            }
    substitution = sortSubstitution aliasParams params
    childSorts = substituteSortVariables substitution <$> sentenceAliasSorts
      where
        SentenceAlias { sentenceAliasSorts } = sentence
    resultSort' = substituteSortVariables substitution sentenceAliasResultSort
    children' = alignWith forceChildSort childSorts children
      where
        forceChildSort =
            \case
                These sort pattern' -> forceSort sort pattern'
                This _ -> (error . unlines) ("Too few arguments." : expected)
                That _ -> (error . unlines) ("Too many arguments." : expected)
        expected =
            [ "Expected:"
            , renderDefault (Unparse.arguments childSorts)
            , "but found:"
            , renderDefault (Unparse.arguments children)
            ]

{- | Construct an 'Application' pattern from a 'Alias' declaration.

The 'Alias' must not be declared with sort parameters.

See also: 'mkApp', 'applyAlias'

 -}
applyAlias_
    ::  ( Traversable domain
        , Ord (variable level)
        , Unparse pattern'
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => SentenceAlias level pattern'
    -> [pattern']
    -> pattern'
applyAlias_ sentence = applyAlias sentence []

{- | Construct an 'Application' pattern from a 'Symbol' declaration.

The provided sort parameters must match the declaration.

See also: 'mkApp', 'applySymbol_', 'mkSymbol'

 -}
applySymbol
    ::  ( Traversable domain
        , Ord (variable level)
        , Unparse pattern'
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => SentenceSymbol level pattern'
    -- ^ 'Symbol' declaration
    -> [Sort level]
    -- ^ 'Symbol' sort parameters
    -> [pattern']
    -- ^ 'Application' arguments
    -> pattern'
applySymbol sentence params children =
    mkApp
        resultSort'
        symbolOrAlias
        children'
  where
    SentenceSymbol { sentenceSymbolSymbol = symbol } = sentence
    SentenceSymbol { sentenceSymbolResultSort } = sentence
    Symbol { symbolConstructor } = symbol
    Symbol { symbolParams } = symbol
    symbolOrAlias =
        SymbolOrAlias
            { symbolOrAliasConstructor = symbolConstructor
            , symbolOrAliasParams = params
            }
    substitution = sortSubstitution symbolParams params
    resultSort' = substituteSortVariables substitution sentenceSymbolResultSort
    childSorts = substituteSortVariables substitution <$> sentenceSymbolSorts
      where
        SentenceSymbol { sentenceSymbolSorts } = sentence
    children' = alignWith forceChildSort childSorts children
      where
        forceChildSort =
            \case
                These sort pattern' -> forceSort sort pattern'
                This _ -> (error . unlines) ("Too few arguments." : expected)
                That _ -> (error . unlines) ("Too many arguments." : expected)
        expected =
            [ "Expected:"
            , renderDefault (Unparse.arguments childSorts)
            , "but found:"
            , renderDefault (Unparse.arguments children)
            ]

{- | Construct an 'Application' pattern from a 'Symbol' declaration.

The 'Symbol' must not be declared with sort parameters.

See also: 'mkApp', 'applySymbol'

 -}
applySymbol_
    ::  ( Traversable domain
        , Ord (variable level)
        , Unparse pattern'
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => SentenceSymbol level pattern'
    -> [pattern']
    -> pattern'
applySymbol_ sentence = applySymbol sentence []

{- | Construct a 'Bottom' pattern in the given sort.

See also: 'mkBottom_'

 -}
mkBottom
    ::  ( Functor domain
        , valid ~ Valid (variable level) level
        )
    => Sort level
    -> PurePattern level domain variable valid
mkBottom bottomSort =
    asPurePattern (valid :< BottomPattern bottom)
  where
    valid = Valid { patternSort = bottomSort, freeVariables = Set.empty }
    bottom = Bottom { bottomSort }

{- | Construct a 'Bottom' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use
'mkBottom' instead.

See also: 'mkBottom'

 -}
mkBottom_
    ::  ( Functor domain
        , valid ~ Valid (variable level) level
        )
    => PurePattern level domain variable valid
mkBottom_ = mkBottom predicateSort

{- | Construct a 'Ceil' pattern in the given sort.

See also: 'mkCeil_'

 -}
mkCeil
    ::  ( Functor domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => Sort level
    -> pattern'
    -> pattern'
mkCeil ceilResultSort ceilChild =
    asPurePattern (valid :< CeilPattern ceil)
  where
    Valid { patternSort = ceilOperandSort, freeVariables } = extract ceilChild
    valid = Valid { patternSort = ceilResultSort, freeVariables }
    ceil = Ceil { ceilOperandSort, ceilResultSort, ceilChild }

{- | Construct a 'Ceil' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use 'mkCeil'
instead.

See also: 'mkCeil'

 -}
mkCeil_
    :: ( Functor domain
       , valid ~ Valid (variable level) level
       , pattern' ~ PurePattern level domain variable valid
       )
    => pattern'
    -> pattern'
mkCeil_ = mkCeil predicateSort

{- | Construct a 'DomainValue' pattern.
 -}
mkDomainValue
    ::  ( Foldable domain
        , Functor domain
        , Ord (variable Object)
        , valid ~ Valid (variable Object) Object
        , pattern' ~ PurePattern Object domain variable valid
        )
    => Sort Object
    -> domain pattern'
    -> pattern'
mkDomainValue domainValueSort domainValueChild =
    asPurePattern (valid :< DomainValuePattern domainValue)
  where
    freeVariables =
        (Set.unions . toList)
            (Valid.freeVariables . extract <$> domainValueChild)
    valid = Valid { patternSort = domainValueSort, freeVariables }
    domainValue = DomainValue { domainValueSort, domainValueChild }

{- | Construct an 'Equals' pattern in the given sort.

See also: 'mkEquals_'

 -}
mkEquals
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => Sort level
    -> pattern'
    -> pattern'
    -> pattern'
mkEquals equalsResultSort =
    makeSortsAgree mkEquals'Worker
  where
    mkEquals'Worker equalsFirst equalsSecond equalsOperandSort =
        asPurePattern (valid :< EqualsPattern equals)
      where
        valid = Valid { patternSort = equalsResultSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract equalsFirst
            Valid { freeVariables = freeVariables2 } = extract equalsSecond
        equals =
            Equals
                { equalsOperandSort
                , equalsResultSort
                , equalsFirst
                , equalsSecond
                }

{- | Construct a 'Equals' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use
'mkEquals' instead.

See also: 'mkEquals'

 -}
mkEquals_
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkEquals_ = mkEquals predicateSort

{- | Construct an 'Exists' pattern.
 -}
mkExists
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => variable level
    -> pattern'
    -> pattern'
mkExists existsVariable existsChild =
    asPurePattern (valid :< ExistsPattern exists)
  where
    freeVariables =
        Set.delete existsVariable freeVariablesChild
      where
        Valid { freeVariables = freeVariablesChild } = extract existsChild
    valid = Valid { patternSort = existsSort, freeVariables }
    existsSort = getSort existsChild
    exists = Exists { existsSort, existsVariable, existsChild }

{- | Construct a 'Floor' pattern in the given sort.

See also: 'mkFloor_'

 -}
mkFloor
    ::  ( Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => Sort level
    -> pattern'
    -> pattern'
mkFloor floorResultSort floorChild =
    asPurePattern (valid :< FloorPattern floor')
  where
    valid = Valid { patternSort = floorResultSort, freeVariables }
    Valid { patternSort = floorOperandSort, freeVariables } = extract floorChild
    floor' = Floor { floorOperandSort, floorResultSort, floorChild }

{- | Construct a 'Floor' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use 'mkFloor'
instead.

See also: 'mkFloor'

 -}
mkFloor_
    ::  ( Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => pattern'
    -> pattern'
mkFloor_ = mkFloor predicateSort

{- | Construct a 'Forall' pattern.
 -}
mkForall
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => variable level
    -> pattern'
    -> pattern'
mkForall forallVariable forallChild =
    asPurePattern (valid :< ForallPattern forall)
  where
    valid = Valid { patternSort = forallSort, freeVariables }
    forallSort = getSort forallChild
    freeVariables =
        Set.delete forallVariable freeVariablesChild
      where
        Valid { freeVariables = freeVariablesChild } = extract forallChild
    forall = Forall { forallSort, forallVariable, forallChild }

{- | Construct an 'Iff' pattern.
 -}
mkIff
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkIff = makeSortsAgree mkIffWorker
  where
    mkIffWorker iffFirst iffSecond iffSort =
        asPurePattern (valid :< IffPattern iff')
      where
        valid = Valid { patternSort = iffSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract iffFirst
            Valid { freeVariables = freeVariables2 } = extract iffSecond
        iff' = Iff { iffSort, iffFirst, iffSecond }

{- | Construct an 'Implies' pattern.
 -}
mkImplies
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkImplies = makeSortsAgree mkImpliesWorker
  where
    mkImpliesWorker impliesFirst impliesSecond impliesSort =
        asPurePattern (valid :< ImpliesPattern implies')
      where
        valid = Valid { patternSort = impliesSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract impliesFirst
            Valid { freeVariables = freeVariables2 } = extract impliesSecond
        implies' = Implies { impliesSort, impliesFirst, impliesSecond }

{- | Construct a 'In' pattern in the given sort.

See also: 'mkIn_'

 -}
mkIn
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => Sort level
    -> pattern'
    -> pattern'
    -> pattern'
mkIn inResultSort = makeSortsAgree mkInWorker
  where
    mkInWorker inContainedChild inContainingChild inOperandSort =
        asPurePattern (valid :< InPattern in')
      where
        valid = Valid { patternSort = inResultSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract inContainedChild
            Valid { freeVariables = freeVariables2 } = extract inContainingChild
        in' =
            In
                { inOperandSort
                , inResultSort
                , inContainedChild
                , inContainingChild
                }

{- | Construct a 'In' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use 'mkIn'
instead.

See also: 'mkIn'

 -}
mkIn_
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkIn_ = mkIn predicateSort

{- | Construct a 'Next' pattern.
 -}
mkNext
    ::  ( Traversable domain
        , valid ~ Valid (variable Object) Object
        , pattern' ~ PurePattern Object domain variable valid
        )
    => pattern'
    -> pattern'
mkNext nextChild =
    asPurePattern (valid :< NextPattern next)
  where
    valid = Valid { patternSort = nextSort, freeVariables }
    Valid { patternSort = nextSort, freeVariables } = extract nextChild
    next = Next { nextSort, nextChild }

{- | Construct a 'Not' pattern.
 -}
mkNot
    ::  ( Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        )
    => pattern'
    -> pattern'
mkNot notChild =
    asPurePattern (valid :< NotPattern not')
  where
    valid = Valid { patternSort = notSort, freeVariables }
    Valid { patternSort = notSort, freeVariables } = extract notChild
    not' = Not { notSort, notChild }

{- | Construct an 'Or' pattern.
 -}
mkOr
    ::  ( Ord (variable level)
        , Traversable domain
        , valid ~ Valid (variable level) level
        , pattern' ~ PurePattern level domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkOr = makeSortsAgree mkOrWorker
  where
    mkOrWorker orFirst orSecond orSort =
        asPurePattern (valid :< OrPattern or')
      where
        valid = Valid { patternSort = orSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract orFirst
            Valid { freeVariables = freeVariables2 } = extract orSecond
        or' = Or { orSort, orFirst, orSecond }

{- | Construct a 'Rewrites' pattern.
 -}
mkRewrites
    ::  ( Ord (variable Object)
        , Traversable domain
        , valid ~ Valid (variable Object) Object
        , pattern' ~ PurePattern Object domain variable valid
        , Unparse pattern'
        )
    => pattern'
    -> pattern'
    -> pattern'
mkRewrites = makeSortsAgree mkRewritesWorker
  where
    mkRewritesWorker rewritesFirst rewritesSecond rewritesSort =
        asPurePattern (valid :< RewritesPattern rewrites')
      where
        valid = Valid { patternSort = rewritesSort, freeVariables }
        freeVariables =
            Set.union freeVariables1 freeVariables2
          where
            Valid { freeVariables = freeVariables1 } = extract rewritesFirst
            Valid { freeVariables = freeVariables2 } = extract rewritesSecond
        rewrites' = Rewrites { rewritesSort, rewritesFirst, rewritesSecond }

{- | Construct a 'Top' pattern in the given sort.

See also: 'mkTop_'

 -}
mkTop
    ::  ( Functor domain
        , valid ~ Valid (variable level) level
        )
    => Sort level
    -> PurePattern level domain variable valid
mkTop topSort =
    asPurePattern (valid :< TopPattern top)
  where
    valid = Valid { patternSort = topSort, freeVariables = Set.empty }
    top = Top { topSort }

{- | Construct a 'Top' pattern in 'predicateSort'.

This should not be used outside "Kore.Predicate.Predicate"; please use
'mkTop' instead.

See also: 'mkTop'

 -}
mkTop_
    ::  ( Functor domain
        , valid ~ Valid (variable level) level
        )
    => PurePattern level domain variable valid
mkTop_ = mkTop predicateSort

{- | Construct a variable pattern.
 -}
mkVar
    ::  ( Functor domain
        , SortedVariable variable
        , valid ~ Valid (variable level) level
        )
    => variable level
    -> PurePattern level domain variable valid
mkVar var =
    asPurePattern (valid :< VariablePattern var)
  where
    patternSort = sortedVariableSort var
    freeVariables = Set.singleton var
    valid = Valid { patternSort, freeVariables }

{- | Construct a 'StringLiteral' pattern.
 -}
mkStringLiteral
    ::  ( Functor domain
        , valid ~ Valid (variable Meta) Meta
        )
    => Text
    -> PurePattern Meta domain variable valid
mkStringLiteral string =
    asPurePattern (valid :< StringLiteralPattern stringLiteral)
  where
    valid = Valid { patternSort = stringMetaSort, freeVariables = Set.empty}
    stringLiteral = StringLiteral string

{- | Construct a 'CharLiteral' pattern.
 -}
mkCharLiteral
    ::  ( Functor domain
        , valid ~ Valid (variable Meta) Meta
        )
    => Char
    -> PurePattern Meta domain variable valid
mkCharLiteral char =
    asPurePattern (valid :< CharLiteralPattern charLiteral)
  where
    valid = Valid { patternSort = charMetaSort, freeVariables = Set.empty }
    charLiteral = CharLiteral char

mkSort :: Id level -> Sort level
mkSort sortActualName =
    SortActualSort SortActual
        { sortActualName
        , sortActualSorts = []
        , sortAttributes = Default.def
        }

mkSortVariable :: Id level -> Sort level
mkSortVariable name = SortVariableSort $ SortVariable name

-- | Construct a variable with a given name and sort
-- "x" `varS` s
varS :: MetaOrObject level => Text -> Sort level -> Variable level
varS x variableSort =
    Variable
        { variableName = noLocationId x
        , variableSort
        , variableCounter = mempty
        }

-- | Construct a symbol with a given name and input sorts
-- "mult" `symS` [s, s]
-- Since the return sort is only found in MetadataTools, this is
-- mostly useful for testing.
symS :: MetaOrObject level => Text -> [Sort level] -> SymbolOrAlias level
symS x s =
    SymbolOrAlias (noLocationId x) s

{- | Construct an axiom declaration with the given parameters and pattern.
 -}
mkAxiom
    ::  ( valid ~ Valid (variable level) level
        , patternType ~ PurePattern level domain variable valid
        )
    => [sortParam]
    -> patternType
    -> SentenceAxiom sortParam patternType
mkAxiom sentenceAxiomParameters sentenceAxiomPattern =
    SentenceAxiom
        { sentenceAxiomParameters
        , sentenceAxiomPattern
        , sentenceAxiomAttributes = Attributes []
        }

{- | Construct an axiom declaration with no parameters.

See also: 'mkAxiom'

 -}
mkAxiom_
    ::  ( valid ~ Valid (variable level) level
        , patternType ~ PurePattern level domain variable valid
        )
    => patternType
    -> SentenceAxiom sortParam patternType
mkAxiom_ = mkAxiom []

{- | Construct a symbol declaration with the given parameters and sorts.
 -}
mkSymbol
    ::  ( valid ~ Valid (variable level) level
        , patternType ~ PurePattern level domain variable valid
        , sortParam ~ SortVariable level
        )
    => Id level
    -> [sortParam]
    -> [Sort level]
    -> Sort level
    -> SentenceSymbol level patternType
mkSymbol symbolConstructor symbolParams argumentSorts resultSort' =
    SentenceSymbol
        { sentenceSymbolSymbol =
            Symbol
                { symbolConstructor
                , symbolParams
                }
        , sentenceSymbolSorts = argumentSorts
        , sentenceSymbolResultSort = resultSort'
        , sentenceSymbolAttributes = Attributes []
        }

{- | Construct a symbol declaration with no parameters.

See also: 'mkSymbol'

 -}
mkSymbol_
    ::  ( valid ~ Valid (variable level) level
        , patternType ~ PurePattern level domain variable valid
        , sortParam ~ SortVariable level
        )
    => Id level
    -> [Sort level]
    -> Sort level
    -> SentenceSymbol level patternType
mkSymbol_ symbolConstructor = mkSymbol symbolConstructor []

{- | Construct an alias declaration with the given parameters and sorts.
 -}
mkAlias
    ::  ( valid ~ Valid (Variable level) level
        , patternType ~ PurePattern level domain Variable valid
        , sortParam ~ SortVariable level
        )
    => Id level
    -> [sortParam]
    -> Sort level
    -> [Variable level]
    -> patternType
    -> SentenceAlias level patternType
mkAlias aliasConstructor aliasParams resultSort' arguments right =
    SentenceAlias
        { sentenceAliasAlias =
            Alias
                { aliasConstructor
                , aliasParams
                }
        , sentenceAliasSorts = argumentSorts
        , sentenceAliasResultSort = resultSort'
        , sentenceAliasLeftPattern =
            Application
                { applicationSymbolOrAlias =
                    SymbolOrAlias
                        { symbolOrAliasConstructor = aliasConstructor
                        , symbolOrAliasParams =
                            SortVariableSort <$> aliasParams
                        }
                , applicationChildren = arguments
                }
        , sentenceAliasRightPattern = right
        , sentenceAliasAttributes = Attributes []
        }
  where
    argumentSorts = variableSort <$> arguments

{- | Construct an alias declaration with no parameters.

See also: 'mkAlias'

 -}
mkAlias_
    ::  ( valid ~ Valid (Variable level) level
        , patternType ~ PurePattern level domain Variable valid
        , sortParam ~ SortVariable level
        )
    => Id level
    -> Sort level
    -> [Variable level]
    -> patternType
    -> SentenceAlias level patternType
mkAlias_ aliasConstructor = mkAlias aliasConstructor []

pattern And_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern App_
    :: Functor dom
    => SymbolOrAlias level
    -> [PurePattern level dom var annotation]
    -> PurePattern level dom var annotation

pattern Bottom_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation

pattern Ceil_
    :: Functor dom
    => Sort level
    -> Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern DV_
  :: Functor dom => (level ~ Object) =>
     Sort level
  -> dom (PurePattern level dom var annotation)
  -> PurePattern level dom var annotation

pattern Equals_
    :: Functor dom
    => Sort level
    -> Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Exists_
    :: Functor dom
    => Sort level
    -> var level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Floor_
    :: Functor dom
    => Sort level
    -> Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Forall_
    :: Functor dom
    => Sort level
    -> var level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Iff_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Implies_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern In_
    :: Functor dom
    => Sort level
    -> Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Next_
    :: Functor dom => (level ~ Object) =>
       Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Not_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Or_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation
    -> PurePattern level dom var annotation

pattern Rewrites_
  :: Functor dom => (level ~ Object) =>
     Sort level
  -> PurePattern level dom var annotation
  -> PurePattern level dom var annotation
  -> PurePattern level dom var annotation

pattern Top_
    :: Functor dom
    => Sort level
    -> PurePattern level dom var annotation

pattern Var_
    :: Functor dom
    => var level
    -> PurePattern level dom var annotation

pattern StringLiteral_
  :: Functor dom => (level ~ Meta)
  => Text
  -> PurePattern level dom var annotation

pattern CharLiteral_
  :: Functor dom => (level ~ Meta)
  => Char
  -> PurePattern level dom var annotation

-- No way to make multiline pragma?
-- NOTE: If you add a case to the AST type, add another synonym here.
{-# COMPLETE And_, App_, Bottom_, Ceil_, DV_, Equals_, Exists_, Floor_, Forall_, Iff_, Implies_, In_, Next_, Not_, Or_, Rewrites_, Top_, Var_, StringLiteral_, CharLiteral_ #-}

pattern And_ andSort andFirst andSecond <-
    (Recursive.project -> _ :< AndPattern And { andSort, andFirst, andSecond })

pattern App_ applicationSymbolOrAlias applicationChildren <-
    (Recursive.project ->
        _ :< ApplicationPattern Application
            { applicationSymbolOrAlias
            , applicationChildren
            }
    )

pattern Bottom_ bottomSort <-
    (Recursive.project -> _ :< BottomPattern Bottom { bottomSort })

pattern Ceil_ ceilOperandSort ceilResultSort ceilChild <-
    (Recursive.project ->
        _ :< CeilPattern Ceil { ceilOperandSort, ceilResultSort, ceilChild }
    )

pattern DV_ domainValueSort domainValueChild <-
    (Recursive.project -> _ :< DomainValuePattern
        DomainValue { domainValueSort, domainValueChild }
    )

pattern Equals_ equalsOperandSort equalsResultSort equalsFirst equalsSecond <-
    (Recursive.project ->
        _ :< EqualsPattern Equals
            { equalsOperandSort
            , equalsResultSort
            , equalsFirst
            , equalsSecond
            }
    )

pattern Exists_ existsSort existsVariable existsChild <-
    (Recursive.project ->
        _ :< ExistsPattern Exists { existsSort, existsVariable, existsChild }
    )

pattern Floor_ floorOperandSort floorResultSort floorChild <-
    (Recursive.project ->
        _ :< FloorPattern Floor
            { floorOperandSort
            , floorResultSort
            , floorChild
            }
    )

pattern Forall_ forallSort forallVariable forallChild <-
    (Recursive.project ->
        _ :< ForallPattern Forall { forallSort, forallVariable, forallChild }
    )

pattern Iff_ iffSort iffFirst iffSecond <-
    (Recursive.project ->
        _ :< IffPattern Iff { iffSort, iffFirst, iffSecond }
    )

pattern Implies_ impliesSort impliesFirst impliesSecond <-
    (Recursive.project ->
        _ :< ImpliesPattern Implies { impliesSort, impliesFirst, impliesSecond }
    )

pattern In_ inOperandSort inResultSort inFirst inSecond <-
    (Recursive.project ->
        _ :< InPattern In
            { inOperandSort
            , inResultSort
            , inContainedChild = inFirst
            , inContainingChild = inSecond
            }
    )

pattern Next_ nextSort nextChild <-
    (Recursive.project ->
        _ :< NextPattern Next { nextSort, nextChild })

pattern Not_ notSort notChild <-
    (Recursive.project ->
        _ :< NotPattern Not { notSort, notChild })

pattern Or_ orSort orFirst orSecond <-
    (Recursive.project -> _ :< OrPattern Or { orSort, orFirst, orSecond })

pattern Rewrites_ rewritesSort rewritesFirst rewritesSecond <-
    (Recursive.project ->
        _ :< RewritesPattern Rewrites
            { rewritesSort
            , rewritesFirst
            , rewritesSecond
            }
    )

pattern Top_ topSort <-
    (Recursive.project -> _ :< TopPattern Top { topSort })

pattern Var_ variable <-
    (Recursive.project -> _ :< VariablePattern variable)

pattern V
    :: Functor dom
    => var level
    -> PurePattern level dom var annotation
pattern V x <- Var_ x

pattern StringLiteral_ str <-
    (Recursive.project -> _ :< StringLiteralPattern (StringLiteral str))

pattern CharLiteral_ char <-
    (Recursive.project -> _ :< CharLiteralPattern (CharLiteral char))
