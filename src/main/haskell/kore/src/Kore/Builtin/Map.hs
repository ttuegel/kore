{- |
Module      : Kore.Builtin.Map
Description : Built-in arbitrary-precision integer sort
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
    , asPattern
    , asExpandedPattern
    , Symbols (..)
    ) where

import qualified Control.Monad as Monad
import           Control.Monad.Except
                 ( ExceptT, runExceptT )
import qualified Control.Monad.Except as Except
import qualified Data.HashMap.Strict as HashMap
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import qualified Kore.AST.Common as Kore
import           Kore.AST.MetaOrObject
                 ( Object )
import           Kore.AST.PureML
                 ( CommonPurePattern )
import qualified Kore.ASTUtils.SmartPatterns as Kore
import qualified Kore.Builtin.Bool as Bool
import qualified Kore.Builtin.Builtin as Builtin
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern )
import qualified Kore.Step.ExpandedPattern as ExpandedPattern
import           Kore.Step.Function.Data
                 ( AttemptedFunction (..) )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern


{- | Builtin name of the @Map@ sort.
 -}
sort :: String
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
sortDeclVerifiers = HashMap.fromList [ (sort, Builtin.verifySortDecl) ]

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

 -}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( "MAP.concat"
      , Builtin.verifySymbol assertSort [assertSort , assertSort]
      )
    , ( "MAP.element"
      , Builtin.verifySymbol assertSort [anySort, anySort]
      )
    , ( "MAP.lookup"
      , Builtin.verifySymbol anySort [assertSort, anySort]
      )
    , ( "MAP.unit"
      , Builtin.verifySymbol assertSort []
      )
    , ( "MAP.update"
      , Builtin.verifySymbol assertSort [assertSort, anySort, anySort]
      )
    , ( "MAP.in_keys"
      , Builtin.verifySymbol Bool.assertSort [assertSort, anySort]
      )
    ]
  where
    anySort :: Builtin.SortVerifier
    anySort = const $ const $ Right ()

type PatternMap = Map (CommonPurePattern Object) (CommonPurePattern Object)

{- | Abort function evaluation if the argument is not a Map domain value.

    If the operand pattern is not a domain value, the function is simply
    'NotApplicatible'. If the operand is a domain value, but not represented
    by a 'BuiltinDomainMap', it is a bug.

 -}
expectBuiltinDomainMap
    :: Monad m
    => String  -- ^ Context for error message
    -> CommonPurePattern Object  -- ^ Operand pattern
    -> ExceptT (AttemptedFunction Object Kore.Variable) m PatternMap
expectBuiltinDomainMap ctx =
    \case
        Kore.DV_ _ domain ->
            case domain of
                Kore.BuiltinDomainMap _map -> return _map
                _ -> Builtin.verifierBug (ctx ++ ": Domain value is not a map")
        _ ->
            Except.throwError NotApplicable

getAttemptedFunction :: Monad m => ExceptT r m r -> m r
getAttemptedFunction = fmap (either id id) . runExceptT

returnMap
    :: Monad m
    => Kore.Sort Object
    -> PatternMap
    -> m (AttemptedFunction Object Kore.Variable)
returnMap resultSort _map =
    Builtin.appliedFunction
        $ ExpandedPattern.fromPurePattern
        $ Kore.DV_ resultSort
        $ Kore.BuiltinDomainMap _map

evalLookup :: Builtin.Function
evalLookup =
    Builtin.functionEvaluator evalLookup0
  where
    evalLookup0 _ _ _ arguments =
        getAttemptedFunction
        (do
            let (_map, _key) =
                    case arguments of
                        [_map, _key] -> (_map, _key)
                        _ -> Builtin.wrongArity "MAP.lookup"
            _map <- expectBuiltinDomainMap "MAP.lookup" _map
            Builtin.appliedFunction
                $ maybe ExpandedPattern.bottom ExpandedPattern.fromPurePattern
                $ Map.lookup _key _map
        )

evalElement :: Builtin.Function
evalElement =
    Builtin.functionEvaluator evalElement0
  where
    evalElement0 _ _ resultSort = \arguments ->
        case arguments of
            [_key, _value] -> returnMap resultSort (Map.singleton _key _value)
            _ -> Builtin.wrongArity "MAP.element"

evalConcat :: Builtin.Function
evalConcat =
    Builtin.functionEvaluator evalConcat0
  where
    evalConcat0 _ _ resultSort = \arguments ->
        getAttemptedFunction
        (do
            let (_map1, _map2) =
                    case arguments of
                        [_map1, _map2] -> (_map1, _map2)
                        _ -> Builtin.wrongArity "MAP.concat"
            _map1 <- expectBuiltinDomainMap "MAP.concat" _map1
            _map2 <- expectBuiltinDomainMap "MAP.concat" _map2
            let overlapping =
                    (not . Set.null)
                    (Set.intersection (Map.keysSet _map1) (Map.keysSet _map2))
            -- Result is ‘\bottom{}()’ when there is overlap between the keys
            -- of the operands.
            (Monad.when overlapping . Except.throwError)
                (Applied $ OrOfExpandedPattern.make [ExpandedPattern.bottom])
            returnMap resultSort (Map.union _map1 _map2)
        )

evalUnit :: Builtin.Function
evalUnit =
    Builtin.functionEvaluator evalUnit0
  where
    evalUnit0 _ _ resultSort =
        \case
            [] -> returnMap resultSort Map.empty
            _ -> Builtin.wrongArity "MAP.unit"

evalUpdate :: Builtin.Function
evalUpdate =
    Builtin.functionEvaluator evalUpdate0
  where
    evalUpdate0 _ _ resultSort = \arguments ->
        getAttemptedFunction
        (do
            let (_map, _key, _value) =
                    case arguments of
                        [_map, _key, _value] -> (_map, _key, _value)
                        _ -> Builtin.wrongArity "MAP.update"
            _map <- expectBuiltinDomainMap "MAP.update" _map
            returnMap resultSort (Map.insert _key _value _map)
        )

evalInKeys :: Builtin.Function
evalInKeys =
    Builtin.functionEvaluator evalInKeys0
  where
    evalInKeys0 _ _ resultSort = \arguments ->
        getAttemptedFunction
        (do
            let (_map, _key) =
                    case arguments of
                        [_map, _key] -> (_map, _key)
                        _ -> Builtin.wrongArity "MAP.in_keys"
            _map <- expectBuiltinDomainMap "MAP.in_keys" _map
            Builtin.appliedFunction
                $ Bool.asExpandedPattern resultSort
                $ Map.member _key _map
        )

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map String Builtin.Function
builtinFunctions =
    Map.fromList
        [ ("MAP.concat", evalConcat)
        , ("MAP.lookup", evalLookup)
        , ("MAP.element", evalElement)
        , ("MAP.unit", evalUnit)
        , ("MAP.update", evalUpdate)
        , ("MAP.in_keys", evalInKeys)
        ]

data Symbols =
    Symbols
        { symbolUnit :: !(Kore.SymbolOrAlias Object)
        , symbolElement :: !(Kore.SymbolOrAlias Object)
        , symbolConcat :: !(Kore.SymbolOrAlias Object)
        , symbolLookup :: !(Kore.SymbolOrAlias Object)
        , symbolUpdate :: !(Kore.SymbolOrAlias Object)
        , symbolInKeys :: !(Kore.SymbolOrAlias Object)
        }

{- | Render a 'Map' as a domain value pattern of the given sort.

  The result sort should be hooked to the builtin @Int@ sort, but this is not
  checked.

  See also: 'sort'

 -}
asPattern
    :: Symbols
    -- ^ dictionary of Map constructor symbols
    -> Kore.Sort Object
    -> Map (CommonPurePattern Object) (CommonPurePattern Object)
    -- ^ builtin value to render
    -> CommonPurePattern Object
asPattern
    Symbols { symbolUnit, symbolUpdate }
    _
    result
  =
    foldr applyUpdate applyUnit (Map.toAscList result)
  where
    applyUnit = Kore.App_ symbolUnit []
    applyUpdate (k, v) m = Kore.App_ symbolUpdate [m, k, v]

asExpandedPattern
    :: Symbols
    -- ^ dictionary of Map constructor symbols
    -> Kore.Sort Object
    -> Map (CommonPurePattern Object) (CommonPurePattern Object)
    -- ^ builtin value to render
    -> CommonExpandedPattern Object
asExpandedPattern symbols resultSort =
    ExpandedPattern.fromPurePattern . asPattern symbols resultSort
