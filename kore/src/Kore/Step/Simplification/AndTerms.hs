{-|
Module      : Kore.Step.Simplification.AndTerms
Description : Unification and "and" simplification for terms.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.AndTerms
    ( termUnification
    , maybeTermAnd
    , maybeTermEquals
    , TermSimplifier
    , TermTransformationOld
    , cannotUnifyDistinctDomainValues
    , functionAnd
    , equalsFunctions
    , andFunctions
    ) where

import Prelude.Kore hiding
    ( concat
    )

import Control.Error
    ( MaybeT (..)
    , mapMaybeT
    )
import qualified Control.Error as Error
import qualified Data.Foldable as Foldable
import qualified Data.Functor.Foldable as Recursive
import Data.String
    ( fromString
    )
import qualified Data.Text as Text

import qualified Kore.Builtin.Bool as Builtin.Bool
import qualified Kore.Builtin.Endianness as Builtin.Endianness
import qualified Kore.Builtin.KEqual as Builtin.KEqual
import qualified Kore.Builtin.List as Builtin.List
import qualified Kore.Builtin.Map as Builtin.Map
import qualified Kore.Builtin.Set as Builtin.Set
import qualified Kore.Builtin.Signedness as Builtin.Signedness
import qualified Kore.Domain.Builtin as Domain
import Kore.Internal.Condition as Condition
import qualified Kore.Internal.MultiOr as MultiOr
import qualified Kore.Internal.OrCondition as OrCondition
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Conditional (..)
    , Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import Kore.Internal.Predicate
    ( pattern PredicateTrue
    , makeEqualsPredicate_
    , makeNotPredicate
    )
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.SideCondition
    ( SideCondition
    )
import qualified Kore.Internal.SideCondition as SideCondition
    ( topTODO
    )
import qualified Kore.Internal.Substitution as Substitution
import qualified Kore.Internal.Symbol as Symbol
import Kore.Internal.TermLike
import Kore.Step.Simplification.CeilSimplifier
    ( CeilSimplifier (..)
    )
import qualified Kore.Step.Simplification.Exists as Exists
import Kore.Step.Simplification.ExpandAlias
    ( expandAlias
    )
import Kore.Step.Simplification.InjSimplifier
import Kore.Step.Simplification.NoConfusion
import Kore.Step.Simplification.NotSimplifier
import Kore.Step.Simplification.Overloading as Overloading
import Kore.Step.Simplification.SimplificationType
    ( SimplificationType
    )
import qualified Kore.Step.Simplification.SimplificationType as SimplificationType
    ( SimplificationType (..)
    )
import Kore.Step.Simplification.Simplify as Simplifier
import Kore.Syntax.PatternF
    ( Const (..)
    )
import Kore.TopBottom
import Kore.Unification.Error
    ( unsupportedPatterns
    )
import Kore.Unification.Unify as Unify
import Kore.Unparser
import qualified Log
import Pair
import qualified Pretty

import {-# SOURCE #-} qualified Kore.Step.Simplification.Ceil as Ceil
    ( makeEvaluateTerm
    )

data SimplificationTarget = AndT | EqualsT | BothT

{- | Unify two terms without discarding the terms.

We want to keep the terms because substitution relies on the result not being
@\\bottom@.

Unlike 'termAnd', @termUnification@ does not make an @\\and@ term when a
particular case is not implemented; otherwise, the two are the same.

The comment for 'Kore.Step.Simplification.And.simplify' describes all
the special cases handled by this.

-}
termUnification
    :: forall variable unifier
    .  InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> TermLike variable
    -> TermLike variable
    -> unifier (Pattern variable)
termUnification notSimplifier =
    termUnificationWorker
  where
    termUnificationWorker
        :: TermLike variable
        -> TermLike variable
        -> unifier (Pattern variable)
    termUnificationWorker pat1 pat2 = do
        let
            maybeTermUnification :: MaybeT unifier (Pattern variable)
            maybeTermUnification =
                maybeTermAnd notSimplifier termUnificationWorker pat1 pat2
            unsupportedPatternsError =
                throwUnificationError
                    (unsupportedPatterns
                        "Unknown unification case."
                        pat1
                        pat2
                    )
        Error.maybeT unsupportedPatternsError pure maybeTermUnification

maybeTermEquals
    :: InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> TermSimplifier variable unifier
    -- ^ Used to simplify subterm "and".
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
maybeTermEquals notSimplifier =
    maybeTransformTerm (equalsFunctions notSimplifier)

maybeTermAnd
    :: InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> TermSimplifier variable unifier
    -- ^ Used to simplify subterm "and".
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
maybeTermAnd notSimplifier =
    maybeTransformTerm (andFunctions notSimplifier)

andFunctions
    :: forall variable unifier
    .  InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> [TermTransformationOld variable unifier]
andFunctions notSimplifier =
    forAnd . snd
    <$> filter appliesToAnd (andEqualsFunctions notSimplifier)
  where
    appliesToAnd :: (SimplificationTarget, a) -> Bool
    appliesToAnd (AndT, _) = True
    appliesToAnd (EqualsT, _) = False
    appliesToAnd (BothT, _) = True

    forAnd
        :: TermTransformation variable unifier
        -> TermTransformationOld variable unifier
    forAnd f = f SideCondition.topTODO SimplificationType.And

equalsFunctions
    :: forall variable unifier
    .  InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> [TermTransformationOld variable unifier]
equalsFunctions notSimplifier =
    forEquals . snd
    <$> filter appliesToEquals (andEqualsFunctions notSimplifier)
  where
    appliesToEquals :: (SimplificationTarget, a) -> Bool
    appliesToEquals (AndT, _) = False
    appliesToEquals (EqualsT, _) = True
    appliesToEquals (BothT, _) = True

    forEquals
        :: TermTransformation variable unifier
        -> TermTransformationOld variable unifier
    forEquals f = f SideCondition.topTODO SimplificationType.Equals

andEqualsFunctions
    :: forall variable unifier
    .  InternalVariable variable
    => MonadUnify unifier
    => HasCallStack
    => NotSimplifier unifier
    -> [(SimplificationTarget, TermTransformation variable unifier)]
andEqualsFunctions notSimplifier = fmap mapEqualsFunctions
    [ (AndT,    \_ _ s -> expandAlias (maybeTermAnd notSimplifier s), "expandAlias")
    , (AndT,    \_ _ _ -> boolAnd, "boolAnd")
    , (BothT,   \_ _ _ -> equalAndEquals, "equalAndEquals")
    , (BothT,   \_ _ _ -> bytesDifferent, "bytesDifferent")
    , (EqualsT, \p _ _ -> bottomTermEquals p, "bottomTermEquals")
    , (EqualsT, \p _ _ -> termBottomEquals p, "termBottomEquals")
    , (BothT,   \p t _ -> variableFunctionAndEquals p t, "variableFunctionAndEquals")
    , (BothT,   \p t _ -> functionVariableAndEquals p t, "functionVariableAndEquals")
    , (BothT,   \_ _ s -> equalInjectiveHeadsAndEquals s, "equalInjectiveHeadsAndEquals")
    , (BothT,   \_ _ s -> sortInjectionAndEquals s, "sortInjectionAndEquals")
    , (BothT,   \_ _ _ -> constructorSortInjectionAndEquals, "constructorSortInjectionAndEquals")
    , (BothT,   \_ _ _ -> constructorAndEqualsAssumesDifferentHeads, "constructorAndEqualsAssumesDifferentHeads")
    , (BothT,   \_ _ s -> overloadedConstructorSortInjectionAndEquals s, "overloadedConstructorSortInjectionAndEquals")
    , (BothT,   \_ _ s -> Builtin.Bool.termAndEquals s, "Builtin.Bool.termAndEquals")
    , (BothT,   \_ _ s -> Builtin.Bool.termNotBool s, "Builtin.Bool.termNotBool")
    , (EqualsT, \_ _ s -> Builtin.KEqual.termKEquals s notSimplifier, "Builtin.KEqual.termKEquals")
    , (AndT,    \_ _ s -> Builtin.KEqual.unifyIfThenElse s, "Builtin.KEqual.unifyIfThenElse")
    , (BothT,   \_ _ _ -> Builtin.Endianness.unifyEquals, "Builtin.Endianness.unifyEquals")
    , (BothT,   \_ _ _ -> Builtin.Signedness.unifyEquals, "Builtin.Signedness.unifyEquals")
    , (BothT,   \_ _ s -> Builtin.Map.unifyEquals s, "Builtin.Map.unifyEquals")
    , (EqualsT, \_ _ s -> Builtin.Map.unifyNotInKeys s notSimplifier ceilSimplifier, "Builtin.Map.unifyNotInKeys")
    , (BothT,   \_ _ s -> Builtin.Set.unifyEquals s, "Builtin.Set.unifyEquals")
    , (BothT,   \_ t s -> Builtin.List.unifyEquals t s, "Builtin.List.unifyEquals")
    , (BothT,   \_ _ _ -> domainValueAndConstructorErrors, "domainValueAndConstructorErrors")
    , (BothT,   \_ _ _ -> domainValueAndEqualsAssumesDifferent, "domainValueAndEqualsAssumesDifferent")
    , (BothT,   \_ _ _ -> stringLiteralAndEqualsAssumesDifferent, "stringLiteralAndEqualsAssumesDifferent")
    , (AndT,    \_ _ _ t1 t2 -> Error.hoistMaybe $ functionAnd t1 t2, "functionAnd")
    ]
  where

    ceilSimplifier =
        CeilSimplifier $ \Ceil { ceilChild } ->
            Ceil.makeEvaluateTerm SideCondition.topTODO ceilChild

    mapEqualsFunctions (target, termTransform, name) =
        (target, logTT name termTransform)

    logTT
        :: String
        -> TermTransformation variable unifier
        -> TermTransformation variable unifier
    logTT fnName termTransformation predicate sType ts t1 t2 =
        mapMaybeT (\getResult -> do
            mresult <- getResult
            case mresult of
                Nothing -> do
                    Log.logDebug . Text.pack . show
                        $ Pretty.hsep
                            [ "Evaluator"
                            , Pretty.pretty fnName
                            , "does not apply."
                            ]
                    return mresult
                Just result -> do
                    Log.logDebug . Text.pack . show
                        $ Pretty.vsep
                            [ Pretty.hsep
                                [ "Evaluator"
                                , Pretty.pretty fnName
                                ]
                            , Pretty.indent 4 $ Pretty.vsep
                                [ "First:"
                                , Pretty.indent 4 $ unparse t1
                                , "Second:"
                                , Pretty.indent 4 $ unparse t2
                                , "Result:"
                                , Pretty.indent 4 $ unparse result
                                ]
                            ]
                    return mresult
            )
            $ termTransformation predicate sType ts t1 t2

{- | Construct the conjunction or unification of two terms.

Each @TermTransformationOld@ should represent one unification case and each
unification case should be handled by only one @TermTransformationOld@. If the
pattern heads do not match the case under consideration, call 'empty' to allow
another case to handle the patterns. If the pattern heads do match the
unification case, then use 'lift' to wrap the implementation
of that case.

All the @TermTransformationOld@s and similar functions defined in this module
call 'empty' unless given patterns matching their unification case.

 -}
type TermTransformation variable unifier =
       SideCondition variable
    -> SimplificationType
    -> TermSimplifier variable unifier
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)

type TermTransformationOld variable unifier =
       TermSimplifier variable unifier
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)

maybeTransformTerm
    :: MonadUnify unifier
    => [TermTransformationOld variable unifier]
    -> TermSimplifier variable unifier
    -- ^ Used to simplify subterm pairs.
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
maybeTransformTerm topTransformers childTransformers first second =
    Foldable.asum
        (map
            (\f -> f
                childTransformers
                first
                second
            )
            topTransformers
        )

-- | Simplify the conjunction of terms where one is a predicate.
boolAnd
    :: MonadUnify unifier
    => InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
boolAnd first second
  | isBottom first  = do
      explainBoolAndBottom first second
      return (Pattern.fromTermLike first)
  | isTop first     = return (Pattern.fromTermLike second)
  | isBottom second = do
      explainBoolAndBottom first second
      return (Pattern.fromTermLike second)
  | isTop second    = return (Pattern.fromTermLike first)
  | otherwise       = empty

explainBoolAndBottom
    :: MonadUnify unifier
    => InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier ()
explainBoolAndBottom term1 term2 =
    lift $ explainBottom "Cannot unify bottom." term1 term2

-- | Unify two identical ('==') patterns.
equalAndEquals
    :: InternalVariable variable
    => Monad unifier
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
equalAndEquals first second
  | first == second =
    return (Pattern.fromTermLike first)
equalAndEquals _ _ = empty

-- | Unify two patterns where the first is @\\bottom@.
bottomTermEquals
    :: InternalVariable variable
    => MonadUnify unifier
    => SideCondition variable
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
bottomTermEquals
    sideCondition
    first@(Bottom_ _)
    second
  = lift $ do -- MonadUnify
    secondCeil <- Ceil.makeEvaluateTerm sideCondition second

    case MultiOr.extractPatterns secondCeil of
        [] -> return Pattern.top
        [ Conditional { predicate = PredicateTrue, substitution } ]
          | substitution == mempty -> do
            explainBottom
                "Cannot unify bottom with non-bottom pattern."
                first
                second
            empty
        _ ->
            return  Conditional
                { term = mkTop_
                , predicate =
                    makeNotPredicate
                    $ OrCondition.toPredicate
                    $ Condition.toPredicate <$> secondCeil
                , substitution = mempty
                }
bottomTermEquals _ _ _ = empty

{- | Unify two patterns where the second is @\\bottom@.

See also: 'bottomTermEquals'

 -}
termBottomEquals
    :: InternalVariable variable
    => MonadUnify unifier
    => SideCondition variable
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
termBottomEquals sideCondition first second =
    bottomTermEquals sideCondition second first

{- | Unify a variable with a function pattern.

See also: 'isFunctionPattern'

 -}
variableFunctionAndEquals
    :: InternalVariable variable
    => MonadUnify unifier
    => SideCondition variable
    -> SimplificationType
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
variableFunctionAndEquals
    _
    SimplificationType.And
    (ElemVar_ v1)
    second@(ElemVar_ _)
  =
      return $ Pattern.assign (inject v1) second
variableFunctionAndEquals
    sideCondition
    simplificationType
    first@(ElemVar_ v)
    second
  | isFunctionPattern second = lift $ do -- MonadUnify
    predicate <-
        case simplificationType of -- Simplifier
            SimplificationType.And ->
                -- Ceil predicate not needed since 'second' being bottom
                -- will make the entire term bottom. However, one must
                -- be careful to not just drop the term.
                return Condition.top
            SimplificationType.Equals -> do
                resultOr <- Ceil.makeEvaluateTerm sideCondition second
                case MultiOr.extractPatterns resultOr of
                    [] -> do
                        explainBottom
                           "Unification of variable and bottom \
                           \when attempting to simplify equals."
                           first
                           second
                        empty
                    resultConditions -> Unify.scatter resultConditions
    let result =
            predicate
            <> Condition.fromSingleSubstitution
                (Substitution.assign (inject v) second)
    return (Pattern.withCondition second result)
variableFunctionAndEquals _ _ _ _ = empty

{- | Unify a function pattern with a variable.

See also: 'variableFunctionAndEquals'

 -}
functionVariableAndEquals
    :: (InternalVariable variable, MonadUnify unifier)
    => SideCondition variable
    -> SimplificationType
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
functionVariableAndEquals sideCondition simplificationType first second =
    variableFunctionAndEquals sideCondition simplificationType second first

{- | Simplify the conjunction of two sort injections.

Assumes that the two heads were already tested for equality and were found
to be different.

This simplifies cases where there is a subsort relation between the injected
sorts of the conjoined patterns, such as,

@
    \inj{src1, dst}(a) ∧ \inj{src2, dst}(b)
    ===
    \inj{src2, dst}(\inj{src1, src2}(a) ∧ b)
@

when @src1@ is a subsort of @src2@.

 -}
sortInjectionAndEquals
    ::  forall variable unifier
    .   ( InternalVariable variable
        , MonadUnify unifier
        )
    => TermSimplifier variable unifier
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
sortInjectionAndEquals termMerger first@(Inj_ inj1) second@(Inj_ inj2) = do
    InjSimplifier { unifyInj } <- Simplifier.askInjSimplifier
    unifyInj inj1 inj2 & either distinct merge
  where
    emptyIntersection = explainAndReturnBottom "Empty sort intersection"
    distinct Distinct = lift $ emptyIntersection first second
    distinct Unknown = empty
    merge inj@Inj { injChild = Pair child1 child2 } = lift $ do
        childPattern <- termMerger child1 child2
        InjSimplifier { evaluateInj } <- askInjSimplifier
        let (childTerm, childCondition) = Pattern.splitTerm childPattern
            inj' = evaluateInj inj { injChild = childTerm }
        return $ Pattern.withCondition inj' childCondition
sortInjectionAndEquals _ _ _ = empty

{- | Unify a constructor application pattern with a sort injection pattern.

Sort injections clash with constructors, so @constructorSortInjectionAndEquals@
returns @\\bottom@.

 -}
constructorSortInjectionAndEquals
    :: InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier a
constructorSortInjectionAndEquals first@(Inj_ _) second@(App_ symbol2 _)
  | Symbol.isConstructor symbol2 =
    lift $ noConfusionInjectionConstructor first second
constructorSortInjectionAndEquals first@(App_ symbol1 _) second@(Inj_ _)
  | Symbol.isConstructor symbol1 =
    lift $ noConfusionInjectionConstructor first second
constructorSortInjectionAndEquals _ _ = empty

noConfusionInjectionConstructor
    :: InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> unifier a
noConfusionInjectionConstructor =
    explainAndReturnBottom "No confusion: sort injections and constructors"

{- |
 If the two constructors form an overload pair, apply the overloading axioms
 on the terms to make the constructors equal, then retry unification on them.

See <https://github.com/kframework/kore/blob/master/docs/2019-08-27-Unification-modulo-overloaded-constructors.md>

 -}
overloadedConstructorSortInjectionAndEquals
    :: (InternalVariable variable, MonadUnify unifier)
    => TermSimplifier variable unifier
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
overloadedConstructorSortInjectionAndEquals termMerger firstTerm secondTerm
  = do
    eunifier <- lift . Error.runExceptT
        $ unifyOverloading (Pair firstTerm secondTerm)
    case eunifier of
        Right (Simple (Pair firstTerm' secondTerm')) -> lift $
            termMerger firstTerm' secondTerm'
        Right
            (WithNarrowing Narrowing
                { narrowingSubst
                , narrowingVars
                , overloadPair = Pair firstTerm' secondTerm'
                }
            ) -> do
                boundPattern <- lift $ do
                    merged <- termMerger firstTerm' secondTerm'
                    Exists.makeEvaluate SideCondition.topTODO narrowingVars
                        $ merged `Pattern.andCondition` narrowingSubst
                case OrPattern.toPatterns boundPattern of
                    [result] -> return result
                    [] -> lift $
                        explainAndReturnBottom
                            (  "exists simplification for overloaded"
                            <> " constructors returned no pattern"
                            )
                            firstTerm
                            secondTerm
                    _ -> empty
        Left (Clash message) -> lift $
            explainAndReturnBottom (fromString message) firstTerm secondTerm
        Left Overloading.NotApplicable -> empty

{- | Unifcation or equality for a domain value pattern vs a constructor
application.

This unification case throws an error because domain values may not occur in a
sort with constructors.

-}
domainValueAndConstructorErrors
    :: InternalVariable variable
    => Monad unifier
    => HasCallStack
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier a
domainValueAndConstructorErrors
    term1@(DV_ _ _)
    term2@(App_ secondHead _)
    | Symbol.isConstructor secondHead =
      error (unlines [ "Cannot handle DomainValue and Constructor:"
                     , unparseToString term1
                     , unparseToString term2
                     ]
            )
domainValueAndConstructorErrors
    term1@(Builtin_ _)
    term2@(App_ secondHead _)
    | Symbol.isConstructor secondHead =
      error (unlines [ "Cannot handle builtin and Constructor:"
                     , unparseToString term1
                     , unparseToString term2
                     ]
            )
domainValueAndConstructorErrors
    term1@(App_ firstHead _)
    term2@(DV_ _ _)
    | Symbol.isConstructor firstHead =
      error (unlines [ "Cannot handle Constructor and DomainValue:"
                     , unparseToString term1
                     , unparseToString term2
                     ]
            )
domainValueAndConstructorErrors
    term1@(App_ firstHead _)
    term2@(Builtin_ _)
    | Symbol.isConstructor firstHead =
      error (unlines [ "Cannot handle Constructor and builtin:"
                     , unparseToString term1
                     , unparseToString term2
                     ]
            )
domainValueAndConstructorErrors _ _ = empty

{- | Unify two domain values.

The two patterns are assumed to be inequal; therefore this case always return
@\\bottom@.

See also: 'equalAndEquals'

-}
-- TODO (thomas.tuegel): This unification case assumes that \dv is injective,
-- but it is not.
domainValueAndEqualsAssumesDifferent
    :: HasCallStack
    => InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier a
domainValueAndEqualsAssumesDifferent
    first@(DV_ _ _)
    second@(DV_ _ _)
  = lift $ cannotUnifyDomainValues first second
domainValueAndEqualsAssumesDifferent
    first@(Builtin_ (Domain.BuiltinInt _))
    second@(Builtin_ (Domain.BuiltinInt _))
  = lift $ cannotUnifyDomainValues first second
domainValueAndEqualsAssumesDifferent
    first@(Builtin_ (Domain.BuiltinString _))
    second@(Builtin_ (Domain.BuiltinString _))
  = lift $ cannotUnifyDomainValues first second
domainValueAndEqualsAssumesDifferent _ _ = empty

cannotUnifyDistinctDomainValues :: Pretty.Doc ()
cannotUnifyDistinctDomainValues = "Cannot unify distinct domain values."

cannotUnifyDomainValues
    :: HasCallStack
    => InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> unifier a
cannotUnifyDomainValues first second =
    assert (first /= second) $ do
        explainBottom
            cannotUnifyDistinctDomainValues
            first
            second
        empty

{-| Unify two literal strings.

The two patterns are assumed to be inequal; therefore this case always returns
@\\bottom@.

See also: 'equalAndEquals'

 -}
stringLiteralAndEqualsAssumesDifferent
    :: HasCallStack
    => InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier a
stringLiteralAndEqualsAssumesDifferent
    first@(StringLiteral_ _)
    second@(StringLiteral_ _)
  = lift $ cannotUnifyDomainValues first second
stringLiteralAndEqualsAssumesDifferent _ _ = empty

{- | Unify any two function patterns.

The function patterns are unified by creating an @\\equals@ predicate.

-}
functionAnd
    :: InternalVariable variable
    => TermLike variable
    -> TermLike variable
    -> Maybe (Pattern variable)
functionAnd first second
  | isFunctionPattern first, isFunctionPattern second =
    return Conditional
        { term = first  -- different for Equals
        -- Ceil predicate not needed since first being
        -- bottom will make the entire term bottom. However,
        -- one must be careful to not just drop the term.
        , predicate =
            Predicate.markSimplified
            $ makeEqualsPredicate_ first second
        , substitution = mempty
        }
  | otherwise = empty

bytesDifferent
    :: InternalVariable variable
    => MonadUnify unifier
    => TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
bytesDifferent
    (Recursive.project -> _ :< InternalBytesF (Const bytesFirst))
    (Recursive.project -> _ :< InternalBytesF (Const bytesSecond))
  | bytesFirst /= bytesSecond
    = return Pattern.bottom
bytesDifferent _ _ = empty
