{- |
Module      : Kore.Builtin.Set
Description : Built-in sets
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Set as Set
@
 -}
module Kore.Builtin.Set
    ( sort
    , assertSort
    , sortDeclVerifiers
    , symbolVerifiers
    , builtinFunctions
    , Builtin
    , returnSet
    , builtinSet
    , asPattern
    , asExpandedPattern
      -- * Symbols
    , lookupSymbolUnit
    , lookupSymbolElement
    , lookupSymbolConcat
    , lookupSymbolIn
    , lookupSymbolDifference
      -- * Keys
    , unitKey
    , unitKeyT
    , elementKey
    , elementKeyT
    , concatKey
    , concatKeyT
    , inKey
    , inKeyT
    , differenceKey
    , differenceKeyT
    , toListKey
    , toListKeyT
    , sizeKey
    , sizeKeyT
      -- * Unification
    , unifyEquals
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Error
                 ( MaybeT )
import           Control.Monad.Counter
import qualified Control.Monad.Trans as Monad.Trans
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )

import           Kore.AST.Pure as Kore
import           Kore.AST.Valid
import           Kore.Attribute.Hook
                 ( Hook )
import qualified Kore.Builtin.Bool as Bool
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.List as List
import qualified Kore.Domain.Builtin as Domain
import qualified Kore.Error as Kore
import           Kore.IndexedModule.IndexedModule
                 ( VerifiedModule )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern, Predicated (..) )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.Function.Data
                 ( AttemptedFunction (..) )
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
                 ( PredicateSubstitutionSimplifier (..),
                 SimplificationProof (..), SimplificationType, Simplifier,
                 StepPatternSimplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import qualified Kore.Step.StepperAttributes as StepperAttributes
import           Kore.Step.Substitution
                 ( normalize )
import           Kore.Unparser
                 ( Unparse )
import           Kore.Variables.Fresh
                 ( FreshVariable )

{- | Builtin name of the @Set@ sort.
 -}
sort :: Text
sort = "SET.Set"

{- | Verify that the sort is hooked to the builtin @Set@ sort.

  See also: 'sort', 'Builtin.verifySort'

 -}
assertSort :: Builtin.SortVerifier
assertSort findSort = Builtin.verifySort findSort sort

{- | Verify that hooked sort declarations are well-formed.

  See also: 'Builtin.verifySortDecl'

 -}
sortDeclVerifiers :: Builtin.SortDeclVerifiers
sortDeclVerifiers = HashMap.fromList [ (sort, Builtin.verifySortDecl) ]

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( concatKeyT
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( elementKeyT
      , Builtin.verifySymbol assertSort [anySort]
      )
    , ( unitKeyT
      , Builtin.verifySymbol assertSort []
      )
    , ( inKeyT
      , Builtin.verifySymbol Bool.assertSort [anySort, assertSort]
      )
    , ( differenceKeyT
      , Builtin.verifySymbol assertSort [assertSort, assertSort]
      )
    , ( toListKeyT
      , Builtin.verifySymbol List.assertSort [assertSort]
      )
    , ( sizeKeyT
      , Builtin.verifySymbol Int.assertSort [assertSort]
      )
    ]
  where
    anySort :: Builtin.SortVerifier
    anySort = const $ const $ Right ()

type Builtin = Set (ConcreteStepPattern Object)

{- | Abort function evaluation if the argument is not a @Set@ domain value.

    If the operand pattern is not a domain value, the function is simply
    'NotApplicable'. If the operand is a domain value, but not represented by a
    'BuiltinDomainSet', it is a bug.

 -}
expectBuiltinSet
    :: Monad m
    => String  -- ^ Context for error message
    -> MetadataTools Object StepperAttributes
    -> StepPattern Object variable  -- ^ Operand pattern
    -> MaybeT m Builtin
expectBuiltinSet ctx tools _set =
    do
        _set <- Builtin.expectNormalConcreteTerm tools _set
        case _set of
            DV_ _ domain ->
                case domain of
                    Domain.BuiltinSet set -> return set
                    _ ->
                        Builtin.verifierBug
                            (ctx ++ ": Domain value is not a set")
            _ ->
                empty

returnSet
    :: (Monad m, Ord (variable Object))
    => Sort Object
    -> Builtin
    -> m (AttemptedFunction Object variable)
returnSet resultSort set =
    Builtin.appliedFunction
    $ ExpandedPattern.fromPurePattern
    $ builtinSet resultSort set

evalElement :: Builtin.Function
evalElement =
    Builtin.functionEvaluator evalElement0
  where
    evalElement0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedFunction
        (case arguments of
            [_elem] -> do
                _elem <- Builtin.expectNormalConcreteTerm tools _elem
                returnSet resultSort (Set.singleton _elem)
            _ -> Builtin.wrongArity elementKey
        )

evalIn :: Builtin.Function
evalIn =
    Builtin.functionEvaluator evalIn0
  where
    evalIn0
        :: Ord (variable Object)
        => MetadataTools Object StepperAttributes
        -> StepPatternSimplifier Object variable
        -> Sort Object
        -> [StepPattern Object variable]
        -> Simplifier (AttemptedFunction Object variable)
    evalIn0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedFunction
        (do
            let (_elem, _set) =
                    case arguments of
                        [_elem, _set] -> (_elem, _set)
                        _ -> Builtin.wrongArity inKey
            _elem <- Builtin.expectNormalConcreteTerm tools _elem
            _set <- expectBuiltinSet inKey tools _set
            (Builtin.appliedFunction . asExpandedBoolPattern)
                (Set.member _elem _set)
        )
      where
        asExpandedBoolPattern = Bool.asExpandedPattern resultSort

evalUnit :: Builtin.Function
evalUnit =
    Builtin.functionEvaluator evalUnit0
  where
    evalUnit0 _ _ resultSort =
        \case
            [] -> returnSet resultSort Set.empty
            _ -> Builtin.wrongArity unitKey

evalConcat :: Builtin.Function
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    ctx = concatKey
    evalConcat0
        :: Ord (variable Object)
        => MetadataTools Object StepperAttributes
        -> StepPatternSimplifier Object variable
        -> Sort Object
        -> [StepPattern Object variable]
        -> Simplifier (AttemptedFunction Object variable)
    evalConcat0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedFunction
        (do
            let (_set1, _set2) =
                    case arguments of
                        [_set1, _set2] -> (_set1, _set2)
                        _ -> Builtin.wrongArity ctx
                leftIdentity = do
                    _set1 <- expectBuiltinSet ctx tools _set1
                    if Set.null _set1
                        then
                            Builtin.appliedFunction
                            $ ExpandedPattern.fromPurePattern _set2
                        else empty
                rightIdentity = do
                    _set2 <- expectBuiltinSet ctx tools _set2
                    if Set.null _set2
                        then
                            Builtin.appliedFunction
                            $ ExpandedPattern.fromPurePattern _set1
                        else empty
                bothConcrete = do
                    _set1 <- expectBuiltinSet ctx tools _set1
                    _set2 <- expectBuiltinSet ctx tools _set2
                    returnSet resultSort (_set1 <> _set2)
            leftIdentity <|> rightIdentity <|> bothConcrete
        )

evalDifference :: Builtin.Function
evalDifference =
    Builtin.functionEvaluator evalDifference0
  where
    ctx = differenceKey
    evalDifference0
        :: Ord (variable Object)
        => MetadataTools Object StepperAttributes
        -> StepPatternSimplifier Object variable
        -> Sort Object
        -> [StepPattern Object variable]
        -> Simplifier (AttemptedFunction Object variable)
    evalDifference0 tools _ resultSort = \arguments ->
        Builtin.getAttemptedFunction
        (do
            let (_set1, _set2) =
                    case arguments of
                        [_set1, _set2] -> (_set1, _set2)
                        _ -> Builtin.wrongArity ctx
                rightIdentity = do
                    _set2 <- expectBuiltinSet ctx tools _set2
                    if Set.null _set2
                        then
                            Builtin.appliedFunction
                            $ ExpandedPattern.fromPurePattern _set1
                        else empty
                bothConcrete = do
                    _set1 <- expectBuiltinSet ctx tools _set1
                    _set2 <- expectBuiltinSet ctx tools _set2
                    returnSet resultSort (Set.difference _set1 _set2)
            rightIdentity <|> bothConcrete
        )

evalToList :: Builtin.Function
evalToList = Builtin.functionEvaluator evalToList0
  where
    evalToList0
        :: Ord (variable Object)
        => MetadataTools Object StepperAttributes
        -> StepPatternSimplifier Object variable
        -> Sort Object
        -> [StepPattern Object variable]
        -> Simplifier (AttemptedFunction Object variable)
    evalToList0 tools _ resultSort arguments =
        Builtin.getAttemptedFunction $ do
            let _set =
                        case arguments of
                            [_set] -> _set
                            _      -> Builtin.wrongArity toListKey
            _set <- expectBuiltinSet toListKey tools _set
            List.returnList resultSort
                . fmap fromConcreteStepPattern
                . Seq.fromList
                . Set.toList
                $ _set

evalSize :: Builtin.Function
evalSize = Builtin.functionEvaluator evalSize0
  where
    evalSize0
        :: Ord (variable Object)
        => MetadataTools Object StepperAttributes
        -> StepPatternSimplifier Object variable
        -> Sort Object
        -> [StepPattern Object variable]
        -> Simplifier (AttemptedFunction Object variable)
    evalSize0 tools _ resultSort arguments =
        Builtin.getAttemptedFunction $ do
            let _set =
                        case arguments of
                            [_set] -> _set
                            _      -> Builtin.wrongArity sizeKey
            _set <- expectBuiltinSet sizeKey tools _set
            Builtin.appliedFunction
                . Int.asExpandedPattern resultSort
                . toInteger
                . Set.size
                $ _set

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (concatKeyT, evalConcat)
        , (elementKeyT, evalElement)
        , (unitKeyT, evalUnit)
        , (inKeyT, evalIn)
        , (differenceKeyT, evalDifference)
        , (toListKeyT, evalToList)
        , (sizeKeyT, evalSize)
        ]

{- | Render a 'Set' as an internal domain value pattern of the given sort.

The result sort should be hooked to the builtin @Set@ sort, but this is not
checked.

The pattern will use the internal representation of concrete 'Set' domain
values; it will not use a valid external representation. Use 'asPattern' to
construct an externally-valid pattern.

 -}
builtinSet
    :: Ord (variable Object)
    => Sort Object
    -> Builtin
    -> StepPattern Object variable
builtinSet resultSort = mkDomainValue resultSort . Domain.BuiltinSet

{- | Render a 'Set' as a domain value pattern of the given sort.

The result sort should be hooked to the builtin @Set@ sort, but this is not
checked.

The constructed pattern will be valid in the contexed of the given indexed
module. It is an error if the indexed module does not define symbols hooked
to @SET.unit@, @SET.element@, and @SET.concat@.

See also: 'sort'

 -}
asPattern
    :: Ord (variable Object)
    => VerifiedModule declAttrs axiomAttrs
    -- ^ indexed module where pattern would appear
    -> Sort Object
    -> Either
        (Kore.Error e)
        (Builtin -> StepPattern Object variable)
asPattern indexedModule dvSort = do
    symbolUnit <- lookupSymbolUnit dvSort indexedModule
    let applyUnit = mkApp dvSort symbolUnit []
    symbolElement <- lookupSymbolElement dvSort indexedModule
    let applyElement elem' =
            mkApp dvSort symbolElement [fromConcreteStepPattern elem']
    symbolConcat <- lookupSymbolConcat dvSort indexedModule
    let
        applyConcat set1 set2 = mkApp dvSort symbolConcat [set1, set2]
        asPattern0 set =
            foldr applyConcat applyUnit (applyElement <$> Foldable.toList set)
    return asPattern0

{- | Render a 'Seq' as an extended domain value pattern.

    See also: 'asPattern'

 -}
asExpandedPattern
    :: Ord (variable Object)
    => VerifiedModule declAttrs axiomAttrs
    -- ^ dictionary of Map constructor symbols
    -> Sort Object
    -> Either
        (Kore.Error e)
        (Builtin -> ExpandedPattern Object variable)
asExpandedPattern symbols resultSort =
    asExpandedPattern0 <$> asPattern symbols resultSort
  where
    asExpandedPattern0 = \asPattern0 builtin ->
        ExpandedPattern.fromPurePattern $ asPattern0 builtin

concatKey :: String
concatKey = "SET.concat"
concatKeyT :: Text
concatKeyT = "SET.concat"

elementKey :: String
elementKey = "SET.element"
elementKeyT :: Text
elementKeyT = "SET.element"

unitKey :: String
unitKey = "SET.unit"
unitKeyT :: Text
unitKeyT = "SET.unit"

inKey :: String
inKey = "SET.in"
inKeyT :: Text
inKeyT = "SET.in"

differenceKey :: String
differenceKey = "SET.difference"
differenceKeyT :: Text
differenceKeyT = "SET.difference"

toListKey :: String
toListKey = "SET.set2list"
toListKeyT :: Text
toListKeyT = "SET.set2list"

sizeKey :: String
sizeKey = "SET.size"
sizeKeyT :: Text
sizeKeyT = "SET.size"

{- | Find the symbol hooked to @SET.unit@ in an indexed module.
 -}
lookupSymbolUnit
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolUnit = Builtin.lookupSymbol unitKeyT

{- | Find the symbol hooked to @SET.element@ in an indexed module.
 -}
lookupSymbolElement
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolElement = Builtin.lookupSymbol elementKeyT

{- | Find the symbol hooked to @SET.concat@ in an indexed module.
 -}
lookupSymbolConcat
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolConcat = Builtin.lookupSymbol concatKeyT

{- | Find the symbol hooked to @SET.get@ in an indexed module.
 -}
lookupSymbolIn
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolIn = Builtin.lookupSymbol inKeyT

{- | Find the symbol hooked to @SET.difference@ in an indexed module.
 -}
lookupSymbolDifference
    :: Sort Object
    -> VerifiedModule declAttrs axiomAttrs
    -> Either (Kore.Error e) (SymbolOrAlias Object)
lookupSymbolDifference = Builtin.lookupSymbol differenceKeyT

{- | Check if the given symbol is hooked to @SET.concat@.
 -}
isSymbolConcat
    :: MetadataTools Object Hook
    -> SymbolOrAlias Object
    -> Bool
isSymbolConcat = Builtin.isSymbol concatKeyT

{- | Check if the given symbol is hooked to @SET.element@.
 -}
isSymbolElement
    :: MetadataTools Object Hook
    -> SymbolOrAlias Object
    -> Bool
isSymbolElement = Builtin.isSymbol elementKeyT

{- | Simplify the conjunction or equality of two concrete Set domain values.

    When it is used for simplifying equality, one should separately solve the
    case ⊥ = ⊥. One should also throw away the term in the returned pattern.

    The sets are assumed to have the same sort, but this is not checked. If
    multiple sorts are hooked to the same builtin domain, the verifier should
    reject the definition.
 -}
unifyEquals
    :: forall level variable m p expanded proof.
        ( OrdMetaOrObject variable, ShowMetaOrObject variable
        , SortedVariable variable
        , Unparse (variable level)
        , MonadCounter m
        , MetaOrObject level
        , FreshVariable variable
        , p ~ StepPattern level variable
        , expanded ~ ExpandedPattern level variable
        , proof ~ SimplificationProof level
        )
    => SimplificationType
    -> MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level m
    -> (p -> p -> m (expanded, proof))
    -> (p -> p -> MaybeT m (expanded, proof))
unifyEquals
    simplificationType
    tools
    substitutionSimplifier
    unifyEqualsChildren
  =
    unifyEquals0
  where
    hookTools = StepperAttributes.hook <$> tools

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
        -> MaybeT m (expanded, proof)
    unifyEquals0
        (DV_ resultSort (Domain.BuiltinSet set1))
        (DV_ _    (Domain.BuiltinSet set2))
      =
        Monad.Trans.lift (unifyEqualsConcrete resultSort set1 set2)

    unifyEquals0
        dv1@(DV_ resultSort (Domain.BuiltinSet set1))
        app2@(App_ symbol2 args2)
      | isSymbolConcat hookTools symbol2 =
        Monad.Trans.lift
           (case args2 of
                [DV_ _ (Domain.BuiltinSet set2), x@(Var_ _)] ->
                    unifyEqualsFramed resultSort set1 set2 x
                [x@(Var_ _), DV_ _ (Domain.BuiltinSet set2)] ->
                    unifyEqualsFramed resultSort set1 set2 x
                _ ->
                    Builtin.unifyEqualsUnsolved
                        simplificationType
                        dv1
                        app2
           )
      | isSymbolElement hookTools symbol2 =
        Monad.Trans.lift
            (case args2 of
                [ key2 ] ->
                    -- The key is not concrete yet, or SET.element would
                    -- have evaluated to a domain value.
                    unifyEqualsElement resultSort set1 symbol2 key2
                _ ->
                    Builtin.wrongArity "SET.element"
            )
      | otherwise =
        empty

    unifyEquals0 app_@(App_ _ _) dv_@(DV_ _ _) = unifyEquals0 dv_ app_

    unifyEquals0 _ _ = empty

    -- | Unify two concrete sets
    unifyEqualsConcrete
        :: (level ~ Object, k ~ ConcreteStepPattern Object)
        => Sort level -- ^ Sort of result
        -> Set.Set k
        -> Set.Set k
        -> m (expanded, proof)
    unifyEqualsConcrete resultSort set1 set2
      | set1 == set2 =
        return (unified, SimplificationProof)
      | otherwise =
        return (ExpandedPattern.bottom, SimplificationProof)
      where
        unified =
            (pure . mkDomainValue resultSort . Domain.BuiltinSet)
                set1

    -- | Unify one concrete set with one framed concrete set.
    unifyEqualsFramed
        :: (level ~ Object, k ~ ConcreteStepPattern Object)
        => Sort level  -- ^ Sort of result
        -> Set.Set k  -- ^ concrete set
        -> Set.Set k -- ^ framed concrete set
        -> StepPattern level variable  -- ^ framing variable
        -> m (expanded, proof)
    unifyEqualsFramed resultSort set1 set2 var
      | Set.isSubsetOf set2 set1 = do
        (remainder, _) <-
            unifyEqualsChildren var
            $ asBuiltinDomainSet
            $ Set.difference set1 set2
        let result =
                -- Return the concrete set, but capture any predicates and
                -- substitutions from unifying the framing variable.
                asBuiltinDomainSet set1 <$ remainder
        normalized <- normalize tools substitutionSimplifier result
        return (normalized, SimplificationProof)

      | otherwise =
        return (ExpandedPattern.bottom, SimplificationProof)
      where
        asBuiltinDomainSet = mkDomainValue resultSort . Domain.BuiltinSet

    unifyEqualsElement
        :: forall k . (level ~ Object, k ~ ConcreteStepPattern Object)
        => Sort level
        -> Set k  -- ^ concrete set
        -> SymbolOrAlias level  -- ^ 'element' symbol
        -> p  -- ^ key
        -> m (expanded, proof)
    unifyEqualsElement resultSort set1 element' key2 =
        case Set.toList set1 of
            [fromConcreteStepPattern -> key1] ->
                do
                    (key, _) <- unifyEqualsChildren key1 key2
                    let result =
                            mkApp resultSort element'
                                <$> propagatePredicates [key]
                    return (result, SimplificationProof)
            _ ->
                -- Cannot unify a non-element Set with an element Set.
                return (ExpandedPattern.bottom, SimplificationProof)
