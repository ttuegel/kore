{- |
Module      : Kore.Builtin.Map
Description : Built-in key-value maps
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Map as Map
@
 -}
module Kore.Builtin.Map
    ( sort
    , sortDeclVerifiers
    , symbolVerifiers
    , builtinFunctions
    , Builtin
    , asPattern
    , asInternal
    , asExpandedPattern
      -- * Symbols
    , lookupSymbolUpdate
    , lookupSymbolLookup
    , lookupSymbolInKeys
    , lookupSymbolKeys
    , isSymbolConcat
    , isSymbolElement
    , isSymbolUnit
      -- * keys
    , concatKey
    , lookupKey
    , elementKey
    , unitKey
    , updateKey
    , in_keysKey
    , keysKey
    -- * Unification
    , unifyEquals
    -- * Raw evaluators
    , evalConcat
    , evalElement
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Error
                 ( MaybeT )
import qualified Control.Monad.Trans as Monad.Trans
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Reflection
                 ( Given )
import qualified Data.Reflection as Reflection
import qualified Data.Set as Set
import           Data.String
                 ( IsString )
import           Data.Text
                 ( Text )
import qualified Data.Text as Text

import           Kore.AST.Pure as Kore
import           Kore.AST.Sentence
import           Kore.AST.Valid
import           Kore.Attribute.Hook
                 ( Hook )
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import qualified Kore.Attribute.Symbol as StepperAttributes
import qualified Kore.Builtin.Bool as Bool
import           Kore.Builtin.Builtin
                 ( acceptAnySort )
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Set as Builtin.Set
import qualified Kore.Domain.Builtin as Domain
import qualified Kore.Error as Kore
import           Kore.IndexedModule.IndexedModule
                 ( VerifiedModule )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import           Kore.Step.Axiom.Data
                 ( AttemptedAxiom (..), BuiltinAndAxiomSimplifierMap )
import           Kore.Step.Pattern
import           Kore.Step.Representation.ExpandedPattern
                 ( ExpandedPattern, Predicated (..) )
import qualified Kore.Step.Representation.ExpandedPattern as ExpandedPattern
import           Kore.Step.Simplification.Data
import           Kore.Unification.Unify
                 ( MonadUnify )
import qualified Kore.Unification.Unify as Monad.Unify
import           Kore.Unparser
                 ( Unparse )
import           Kore.Variables.Fresh

{- | Builtin name of the @Map@ sort.
 -}
sort :: Text
sort = "MAP.Map"

{- | Verify that the sort is hooked to the builtin @Int@ sort.

  See also: 'sort', 'Builtin.verifySort'

 -}
assertSort :: Builtin.SortVerifier
assertSort findSort = Builtin.verifySort findSort sort

{- | Verify that hooked sort declarations are well-formed.

  See also: 'Builtin.verifySortDecl'

 -}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers =
    HashMap.fromList [ (sort, verifySortDecl) ]
  where
    verifySortDecl indexedModule sentenceSort attrs = do
        Builtin.verifySortDecl indexedModule sentenceSort attrs
        unitId <- Builtin.getUnitId attrs
        Builtin.assertSymbolHook indexedModule unitId unitKey
        Builtin.assertSymbolResultSort indexedModule unitId expectedSort
        elementId <- Builtin.getElementId attrs
        Builtin.assertSymbolHook indexedModule elementId elementKey
        Builtin.assertSymbolResultSort indexedModule elementId expectedSort
        concatId <- Builtin.getConcatId attrs
        Builtin.assertSymbolHook indexedModule concatId concatKey
        Builtin.assertSymbolResultSort indexedModule concatId expectedSort
        return ()
      where
        SentenceSort { sentenceSortName } = sentenceSort
        expectedSort = mkSort sentenceSortName

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( concatKey
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( elementKey
      , Builtin.verifySymbol assertSort [acceptAnySort, acceptAnySort]
      )
    , ( lookupKey
      , Builtin.verifySymbol acceptAnySort [assertSort, acceptAnySort]
      )
    , ( unitKey
      , Builtin.verifySymbol assertSort []
      )
    , ( updateKey
      , Builtin.verifySymbol assertSort [assertSort, acceptAnySort, acceptAnySort]
      )
    , ( in_keysKey
      , Builtin.verifySymbol Bool.assertSort [acceptAnySort, assertSort]
      )
    , ( keysKey
      , Builtin.verifySymbol Builtin.Set.assertSort [assertSort]
      )
    ]

type Builtin variable =
    Map (ConcreteStepPattern Object) (StepPattern Object variable)

{- | Abort function evaluation if the argument is not a Map domain value.

    If the operand pattern is not a domain value, the function is simply
    'NotApplicable'. If the operand is a domain value, but not represented
    by a 'BuiltinDomainMap', it is a bug.

 -}
expectBuiltinMap
    :: Monad m
    => Text  -- ^ Context for error message
    -> StepPattern Object variable  -- ^ Operand pattern
    -> MaybeT m (Builtin variable)
expectBuiltinMap ctx _map =
    do
        case _map of
            DV_ _ domain ->
                case domain of
                    Domain.BuiltinMap Domain.InternalMap { builtinMapChild } ->
                        return builtinMapChild
                    _ ->
                        Builtin.verifierBug
                        $ Text.unpack ctx ++ ": Domain value is not a map"
            _ ->
                empty

returnMap
    :: (Monad m, Ord (variable Object))
    => MetadataTools Object attrs
    -> Sort Object
    -> Builtin variable
    -> m (AttemptedAxiom Object variable)
returnMap tools resultSort map' =
    Builtin.appliedFunction
    $ ExpandedPattern.fromPurePattern
    $ asInternal tools resultSort map'

evalLookup :: Builtin.Function
evalLookup =
    Builtin.functionEvaluator evalLookup0
  where
    evalLookup0 :: Builtin.FunctionImplementation
    evalLookup0 tools _ _ arguments =
        Builtin.getAttemptedAxiom
        (do
            let (_map, _key) =
                    case arguments of
                        [_map, _key] -> (_map, _key)
                        _ -> Builtin.wrongArity lookupKey
                emptyMap = do
                    _map <- expectBuiltinMap lookupKey _map
                    if Map.null _map
                        then Builtin.appliedFunction ExpandedPattern.bottom
                        else empty
                bothConcrete = do
                    _key <- Builtin.expectNormalConcreteTerm tools _key
                    _map <- expectBuiltinMap lookupKey _map
                    Builtin.appliedFunction $ maybeBottom $ Map.lookup _key _map
            emptyMap <|> bothConcrete
        )
      where
        maybeBottom =
            maybe ExpandedPattern.bottom ExpandedPattern.fromPurePattern

-- | evaluates the map element builtin.
evalElement :: Builtin.Function
evalElement =
    Builtin.functionEvaluator evalElement0
  where
    evalElement0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
        (do
            let (_key, _value) =
                    case arguments of
                        [_key, _value] -> (_key, _value)
                        _ -> Builtin.wrongArity elementKey
            _key <- Builtin.expectNormalConcreteTerm tools _key
            returnMap tools resultSort (Map.singleton _key _value)
        )

-- | evaluates the map concat builtin.
evalConcat :: Builtin.Function
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    evalConcat0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
        (do
            let (_map1, _map2) =
                    case arguments of
                        [_map1, _map2] -> (_map1, _map2)
                        _ -> Builtin.wrongArity concatKey
                leftIdentity = do
                    _map1 <- expectBuiltinMap concatKey _map1
                    if Map.null _map1
                        then
                            Builtin.appliedFunction
                            $ ExpandedPattern.fromPurePattern _map2
                        else
                            empty
                rightIdentity = do
                    _map2 <- expectBuiltinMap concatKey _map2
                    if Map.null _map2
                        then
                            Builtin.appliedFunction
                            $ ExpandedPattern.fromPurePattern _map1
                        else
                            empty
                bothConcrete = do
                    _map1 <- expectBuiltinMap concatKey _map1
                    _map2 <- expectBuiltinMap concatKey _map2
                    let overlapping =
                            (not . Set.null)
                                (Set.intersection
                                    (Map.keysSet _map1)
                                    (Map.keysSet _map2)
                                )
                    if overlapping
                        then
                            -- Result is ‘\bottom{}()’ when there is overlap
                            -- between the keys of the operands.
                            Builtin.appliedFunction ExpandedPattern.bottom
                        else
                            returnMap tools resultSort (Map.union _map1 _map2)
            leftIdentity <|> rightIdentity <|> bothConcrete
        )

evalUnit :: Builtin.Function
evalUnit =
    Builtin.functionEvaluator evalUnit0
  where
    evalUnit0 tools _ resultSort =
        \case
            [] -> returnMap tools resultSort Map.empty
            _ -> Builtin.wrongArity unitKey

evalUpdate :: Builtin.Function
evalUpdate =
    Builtin.functionEvaluator evalUpdate0
  where
    evalUpdate0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
        (do
            let (_map, _key, value) =
                    case arguments of
                        [_map, _key, value'] -> (_map, _key, value')
                        _ -> Builtin.wrongArity updateKey
            _key <- Builtin.expectNormalConcreteTerm tools _key
            _map <- expectBuiltinMap updateKey _map
            returnMap tools resultSort (Map.insert _key value _map)
        )

evalInKeys :: Builtin.Function
evalInKeys =
    Builtin.functionEvaluator evalInKeys0
  where
    evalInKeys0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
        (do
            let (_key, _map) =
                    case arguments of
                        [_key, _map] -> (_key, _map)
                        _ -> Builtin.wrongArity in_keysKey
            _key <- Builtin.expectNormalConcreteTerm tools _key
            _map <- expectBuiltinMap in_keysKey _map
            Builtin.appliedFunction
                $ Bool.asExpandedPattern resultSort
                $ Map.member _key _map
        )

evalKeys :: Builtin.Function
evalKeys =
    Builtin.functionEvaluator evalKeys0
  where
    evalKeys0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedAxiom
        (do
            let _map =
                    case arguments of
                        [_map] -> _map
                        _ -> Builtin.wrongArity lookupKey
            _map <- expectBuiltinMap lookupKey _map
            Builtin.Set.returnSet tools resultSort (Map.keysSet _map)
        )

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (concatKey, evalConcat)
        , (lookupKey, evalLookup)
        , (elementKey, evalElement)
        , (unitKey, evalUnit)
        , (updateKey, evalUpdate)
        , (in_keysKey, evalInKeys)
        , (keysKey, evalKeys)
        ]

{- | Render a 'Map' as an internal pattern of the given sort.

The result sort must be hooked to the builtin @Map@ sort.

See also: 'sort'

 -}
asInternal
    :: Ord (variable Object)
    => MetadataTools Object attrs
    -> Sort Object
    -> Builtin variable
    -> StepPattern Object variable
asInternal tools builtinMapSort builtinMapChild =
    (mkDomainValue . Domain.BuiltinMap)
        Domain.InternalMap
            { builtinMapSort
            , builtinMapUnit =
                Builtin.lookupSymbolUnit builtinMapSort attrs
            , builtinMapElement =
                Builtin.lookupSymbolElement builtinMapSort attrs
            , builtinMapConcat =
                Builtin.lookupSymbolConcat builtinMapSort attrs
            , builtinMapChild
            }
  where
    attrs = sortAttributes tools builtinMapSort

{- | Render a 'Map' as a domain value pattern of the given sort.

The result sort must be hooked to the builtin @Map@ sort.

See also: 'sort'

 -}
asPattern
    :: Ord (variable Object)
    => Domain.InternalMap (StepPattern Object variable)
    -> StepPattern Object variable
asPattern builtin =
    foldr concat' unit (element <$> Map.toAscList map')
  where
    Domain.InternalMap { builtinMapSort = builtinSort } = builtin
    Domain.InternalMap { builtinMapChild = map' } = builtin
    Domain.InternalMap { builtinMapUnit = unitSymbol } = builtin
    Domain.InternalMap { builtinMapElement = elementSymbol } = builtin
    Domain.InternalMap { builtinMapConcat = concatSymbol } = builtin

    apply = mkApp builtinSort
    unit = apply unitSymbol []
    element (key, value) =
        apply elementSymbol [fromConcreteStepPattern key, value]
    concat' map1 map2 = apply concatSymbol [map1, map2]

{- | Render a 'Map' as an extended domain value pattern.

    See also: 'asPattern'

 -}
asExpandedPattern
    ::  ( Ord (variable Object)
        , Given (MetadataTools Object StepperAttributes)
        )
    => Kore.Sort Object
    -> Builtin variable
    -> ExpandedPattern Object variable
asExpandedPattern resultSort =
    ExpandedPattern.fromPurePattern . asInternal tools resultSort
  where
    tools :: MetadataTools Object StepperAttributes
    tools = Reflection.given

concatKey :: IsString s => s
concatKey = "MAP.concat"

lookupKey :: IsString s => s
lookupKey = "MAP.lookup"

elementKey :: IsString s => s
elementKey = "MAP.element"

unitKey :: IsString s => s
unitKey = "MAP.unit"

updateKey :: IsString s => s
updateKey = "MAP.update"

in_keysKey :: IsString s => s
in_keysKey = "MAP.in_keys"

keysKey :: IsString s => s
keysKey = "MAP.keys"

{- | Find the symbol hooked to @MAP.update@ in an indexed module.
 -}
lookupSymbolUpdate
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolUpdate = Builtin.lookupSymbol updateKey

{- | Find the symbol hooked to @MAP.lookup@ in an indexed module.
 -}
lookupSymbolLookup
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolLookup = Builtin.lookupSymbol lookupKey

{- | Find the symbol hooked to @MAP.in_keys@ in an indexed module.
 -}
lookupSymbolInKeys
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolInKeys = Builtin.lookupSymbol in_keysKey

{- | Find the symbol hooked to @MAP.keys@ in an indexed module.
 -}
lookupSymbolKeys
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolKeys = Builtin.lookupSymbol keysKey

{- | Check if the given symbol is hooked to @MAP.concat@.
 -}
isSymbolConcat
    :: MetadataTools Object Hook
    -> SymbolOrAlias Object
    -> Bool
isSymbolConcat = Builtin.isSymbol concatKey

{- | Check if the given symbol is hooked to @MAP.element@.
 -}
isSymbolElement
    :: MetadataTools Object Hook
    -> SymbolOrAlias Object
    -> Bool
isSymbolElement = Builtin.isSymbol elementKey

{- | Check if the given symbol is hooked to @MAP.unit@.
-}
isSymbolUnit
    :: MetadataTools Object Hook
    -> SymbolOrAlias Object
    -> Bool
isSymbolUnit = Builtin.isSymbol unitKey


{- | Simplify the conjunction or equality of two concrete Map domain values.

When it is used for simplifying equality, one should separately solve the
case ⊥ = ⊥. One should also throw away the term in the returned pattern.

The maps are assumed to have the same sort, but this is not checked. If
multiple sorts are hooked to the same builtin domain, the verifier should
reject the definition.

The most general form of the unification problem is
@
(m₁ + x₁) ∧ (m₂ + x₂)
@
where @+@ represents @concat@ and @m₁@, @m₂@ are concrete maps.
The solution is to introduce @qᵢ@ and @rᵢ@ such that
@
m₁ = q₁ + r₁
m₂ = q₂ + r₂
keys(q₁) = keys(q₂)
keys(r₁) ∧ keys(r₂) = ⊥
@
so that
@
(m₁ + x₁) ∧ (m₂ + x₂) = (q₁ ∧ q₂) + (r₁ + x₁) ∧ (r₂ + x₂).
@
When both @x₁@ and @x₂@ are present, we should check that @q₁ ∧ q₂@ is not
empty, otherwise this equation is just a trivial shuffling and does not actually
make progress toward simplification. We introduce special cases when @x₁@ and/or
@x₂@ is missing.
 -}
-- TODO (thomas.tuegel): Handle the case of two framed maps.
unifyEquals
    :: forall level variable unifierM unifier p expanded proof .
        ( OrdMetaOrObject variable, ShowMetaOrObject variable
        , SortedVariable variable
        , MetaOrObject level
        , FreshVariable variable
        , Show (variable level)
        , Unparse (variable level)
        , p ~ StepPattern level variable
        , expanded ~ ExpandedPattern level variable
        , proof ~ SimplificationProof level
        , unifier ~ unifierM variable
        , MonadUnify unifierM
        )
    => SimplificationType
    -> MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level
    -> StepPatternSimplifier level
    -- ^ Evaluates functions.
    -> BuiltinAndAxiomSimplifierMap level
    -- ^ Map from axiom IDs to axiom evaluators
    -> (p -> p -> unifier (expanded, proof))
    -> p
    -> p
    -> MaybeT unifier (expanded, proof)
unifyEquals
    simplificationType
    tools
    _
    _
    _
    unifyEqualsChildren
    first
    second
  =
    unifyEquals0 first second
  where
    hookTools = StepperAttributes.hook <$> tools

    -- | Discard the proofs in a collection of proven expanded patterns.
    discardProofs :: Map k (expanded, proof) -> Map k expanded
    discardProofs = Map.map fst

    -- | Given a collection 't' of 'Predicated' values, propagate all the
    -- predicates to the top level, returning a 'Predicated' collection.
    propagatePredicates
        :: (level ~ Object, Traversable t)
        => t (Predicated level variable a)
        -> Predicated level variable (t a)
    propagatePredicates = sequenceA

    -- | Unify the two argument patterns.
    unifyEquals0
        :: StepPattern level variable
        -> StepPattern level variable
        -> MaybeT unifier (expanded, proof)

    unifyEquals0 dv1@(DV_ _ (Domain.BuiltinMap builtin1)) =
        \case
            dv2@(DV_ _ internal2) ->
                case internal2 of
                    Domain.BuiltinMap builtin2 ->
                        Monad.Trans.lift $ unifyEqualsConcrete builtin1 builtin2
                    _ ->
                        (error . unlines)
                            [ "Cannot unify a builtin Map domain value:"
                            , show dv1
                            , "with:"
                            , show dv2
                            , "This should have been a sort error."
                            ]
            app@(App_ symbol2 args2)
                | isSymbolConcat hookTools symbol2 ->
                    -- Accept the arguments of MAP.concat in either order.
                    Monad.Trans.lift $ case args2 of
                        [ DV_ _ (Domain.BuiltinMap builtin2), x@(Var_ _) ] ->
                            unifyEqualsFramed1 builtin1 builtin2 x
                        [ x@(Var_ _), DV_ _ (Domain.BuiltinMap builtin2) ] ->
                            unifyEqualsFramed1 builtin1 builtin2 x
                        [ _, _ ] ->
                            Builtin.unifyEqualsUnsolved
                                simplificationType
                                dv1
                                app
                        _ ->
                            Builtin.wrongArity "MAP.concat"
                | isSymbolElement hookTools symbol2 ->
                    Monad.Trans.lift $ case args2 of
                        [ key2, value2 ] ->
                            -- The key is not concrete yet, or MAP.element would
                            -- have evaluated to a domain value.
                            unifyEqualsElement
                                builtin1
                                symbol2
                                key2
                                value2
                        _ ->
                            Builtin.wrongArity "MAP.element"
                | otherwise ->
                    empty
            _ ->
                empty

    unifyEquals0 pat1 =
        \case
            dv@(DV_ _ (Domain.BuiltinMap _)) -> unifyEquals0 dv pat1
            _ -> empty

    -- | Unify two concrete maps.
    unifyEqualsConcrete
        :: level ~ Object
        => Domain.InternalMap (StepPattern level variable)
        -> Domain.InternalMap (StepPattern level variable)
        -> unifier (expanded, proof)
    unifyEqualsConcrete builtin1 builtin2 = do
        intersect <-
            sequence (Map.intersectionWith unifyEqualsChildren map1 map2)
        let
            result
              | not (Map.null remainder1) =
                -- There is nothing with which to unify the
                -- remainder of map1.
                bottomWithExplanation
              | not (Map.null remainder2) = bottomWithExplanation
              | otherwise =
                return $ asInternal tools builtinMapSort
                    <$> (propagatePredicates . discardProofs) intersect
              where
                -- Elements of map1 missing from map2
                remainder1 = Map.difference map1 map2
                -- Elements of map2 missing from map1
                remainder2 = Map.difference map2 map1

        (,) <$> result <*> pure SimplificationProof
      where
        Domain.InternalMap { builtinMapSort } = builtin1
        Domain.InternalMap { builtinMapChild = map1 } = builtin1
        Domain.InternalMap { builtinMapChild = map2 } = builtin2

    -- | Unify one concrete map with one framed concrete map.
    unifyEqualsFramed1
        :: level ~ Object
        => Domain.InternalMap (StepPattern Object variable)  -- ^ concrete map
        -> Domain.InternalMap (StepPattern Object variable)  -- ^ framed map
        -> StepPattern Object variable  -- ^ framing variable
        -> unifier (expanded, proof)
    unifyEqualsFramed1 builtin1 builtin2 x = do
        intersect <-
            sequence (Map.intersectionWith unifyEqualsChildren map1 map2)
        -- The framing variable unifies with the remainder of map1.
        let remainder1 = Map.difference map1 map2
        -- The framing part of the unification result.
        (frame, _) <- unifyEqualsChildren x (asBuiltinMap remainder1)
        let
            -- The concrete part of the unification result.
            concrete :: ExpandedPattern level variable
            concrete =
                asBuiltinMap <$> (propagatePredicates . discardProofs) intersect

            result
              | not (Map.null remainder2) = bottomWithExplanation
              | otherwise =
                return $
                    Reflection.give tools asExpandedPattern builtinMapSort map1
                    <* concrete
                    <* frame
              where
                -- Elements of map2 missing from map1
                remainder2 = Map.difference map2 map1

        (,) <$> result <*> pure SimplificationProof
      where
        Domain.InternalMap { builtinMapSort } = builtin1
        Domain.InternalMap { builtinMapChild = map1 } = builtin1
        Domain.InternalMap { builtinMapChild = map2 } = builtin2
        asBuiltinMap = asInternal tools builtinMapSort

    unifyEqualsElement
        :: level ~ Object
        => Domain.InternalMap (StepPattern Object variable)  -- ^ concrete map
        -> SymbolOrAlias level  -- ^ 'element' symbol
        -> StepPattern Object variable  -- ^ key
        -> StepPattern Object variable  -- ^ value
        -> unifier (expanded, proof)
    unifyEqualsElement builtin1 element' key2 value2 =
        case Map.toList map1 of
            [(fromConcreteStepPattern -> key1, value1)] ->
                do
                    (key, _) <- unifyEqualsChildren key1 key2
                    (value, _) <- unifyEqualsChildren value1 value2
                    let result =
                            mkApp builtinMapSort element'
                            <$> propagatePredicates [key, value]
                    return (result, SimplificationProof)
            _ -> withProof bottomWithExplanation
            -- Cannot unify a non-element Map with an element Map
      where
        Domain.InternalMap { builtinMapSort } = builtin1
        Domain.InternalMap { builtinMapChild = map1 } = builtin1
    bottomWithExplanation = do
        Monad.Unify.explainBottom
            "Cannot unify a non-element map with an element map."
            first
            second
        return ExpandedPattern.bottom
    withProof :: Applicative f => f a -> f (a, proof)
    withProof fa = (,) <$> fa <*> pure SimplificationProof
