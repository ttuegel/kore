{-|
Module      : Kore.Step.TranslateSMT
Description : Evaluates conditions.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable
-}

module Kore.Step.TranslateSMT
    ( translatePredicate
    , Translator
    , VarContext
    , evalTranslator
    , runTranslator
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Comonad
import qualified Control.Comonad.Trans.Cofree as Cofree
import           Control.Error
                 ( MaybeT )
import           Control.Monad.Counter
                 ( CounterT, evalCounterT )
import           Control.Monad.Morph as Morph
import           Control.Monad.State.Strict
                 ( StateT, evalStateT )
import qualified Control.Monad.State.Strict as State
import qualified Data.Functor.Foldable as Recursive
import           Data.Map.Strict
                 ( Map )
import qualified Data.Map.Strict as Map
import           Data.Reflection

import           Kore.AST.Kore
import           Kore.AST.Valid
import           Kore.Attribute.Smtlib
import qualified Kore.Attribute.Sort as Attribute
import qualified Kore.Builtin.Bool as Builtin.Bool
import qualified Kore.Builtin.Int as Builtin.Int
import           Kore.IndexedModule.MetadataTools as MetadataTools
import           Kore.Predicate.Predicate
import           Kore.Step.Pattern
import           Kore.Step.StepperAttributes
import           SMT
                 ( SExpr (..), SMT )
import qualified SMT

import Kore.Unparser

-- ----------------------------------------------------------------
-- Predicate translation

{- | Translate a predicate for SMT.

The predicate may inhabit an arbitrary sort. Logical connectives are translated
to their SMT counterparts. Quantifiers, @\\ceil@, @\\floor@, and @\\in@ are
uninterpreted (translated as variables) as is @\\equals@ if its arguments are
not builtins or predicates. All other patterns are not translated and prevent
the predicate from being sent to SMT.

 -}
translatePredicate
    :: forall p variable .
        ( Ord (variable Object)
        , Unparse (variable Object)
        , Given (MetadataTools Object StepperAttributes)
        , p ~ StepPattern Object variable
        )
    => (SExpr -> p -> Translator p SExpr)
    -> Predicate Object variable
    -> Translator p SExpr
translatePredicate translateUninterpreted predicate =
    translatePredicatePattern $ unwrapPredicate predicate
  where
    translatePredicatePattern
        :: StepPattern Object variable
        -> Translator (StepPattern Object variable) SExpr
    translatePredicatePattern pat =
        case Cofree.tailF (Recursive.project pat) of
            -- Logical connectives: translate as connectives
            AndPattern and' -> translatePredicateAnd and'
            BottomPattern _ -> return (SMT.bool False)
            EqualsPattern eq ->
                -- Equality of predicates and builtins can be translated to
                -- equality in the SMT solver, but other patterns must remain
                -- uninterpreted.
                    translatePredicateEquals eq
                <|> translateUninterpreted SMT.tBool pat
            IffPattern iff -> translatePredicateIff iff
            ImpliesPattern implies -> translatePredicateImplies implies
            NotPattern not' -> translatePredicateNot not'
            OrPattern or' -> translatePredicateOr or'
            TopPattern _ -> return (SMT.bool True)

            -- Uninterpreted: translate as variables
            CeilPattern _ -> translateUninterpreted SMT.tBool pat
            ExistsPattern _ -> translateUninterpreted SMT.tBool pat
            FloorPattern _ -> translateUninterpreted SMT.tBool pat
            ForallPattern _ -> translateUninterpreted SMT.tBool pat
            InPattern _ -> translateUninterpreted SMT.tBool pat

            -- Invalid: no translation, should not occur in predicates
            ApplicationPattern _ -> empty
            DomainValuePattern _ -> empty
            NextPattern _ -> empty
            RewritesPattern _ -> empty
            VariablePattern _ -> empty

    translatePredicateAnd And { andFirst, andSecond } =
        SMT.and
            <$> translatePredicatePattern andFirst
            <*> translatePredicatePattern andSecond

    translatePredicateEquals Equals { equalsFirst, equalsSecond } =
        SMT.eq
            <$> translatePredicateEqualsChild equalsFirst
            <*> translatePredicateEqualsChild equalsSecond
      where
        translatePredicateEqualsChild child =
            -- Attempt to translate patterns in builtin sorts, or failing that,
            -- as predicates.
            (<|>)
                (translatePattern child)
                (translatePredicatePattern child)

    translatePredicateIff Iff { iffFirst, iffSecond } =
        iff
            <$> translatePredicatePattern iffFirst
            <*> translatePredicatePattern iffSecond
      where
        iff a b = SMT.and (SMT.implies a b) (SMT.implies b a)

    translatePredicateImplies Implies { impliesFirst, impliesSecond } =
        SMT.implies
            <$> translatePredicatePattern impliesFirst
            <*> translatePredicatePattern impliesSecond

    translatePredicateNot Not { notChild } =
        SMT.not <$> translatePredicatePattern notChild

    translatePredicateOr Or { orFirst, orSecond } =
        SMT.or
            <$> translatePredicatePattern orFirst
            <*> translatePredicatePattern orSecond

    -- | Translate a functional pattern in the builtin Int sort for SMT.
    translateInt
        ::  ( Given (MetadataTools Object StepperAttributes)
            , Ord (variable Object)
            , p ~ StepPattern Object variable
            )
        => p
        -> Translator p SExpr
    translateInt pat =
        case Cofree.tailF (Recursive.project pat) of
            VariablePattern _ -> translateUninterpreted SMT.tInt pat
            DomainValuePattern dv ->
                return $ SMT.int $ Builtin.Int.extractIntDomainValue
                    "while translating dv to SMT.int" dv
            ApplicationPattern app ->
                translateApplication (extract pat) app
            _ -> empty

    -- | Translate a functional pattern in the builtin Bool sort for SMT.
    translateBool
        ::  ( Given (MetadataTools Object StepperAttributes)
            , Ord (variable Object)
            , p ~ StepPattern Object variable
            )
        => p
        -> Translator p SExpr
    translateBool pat =
        case Cofree.tailF (Recursive.project pat) of
            VariablePattern _ -> translateUninterpreted SMT.tBool pat
            DomainValuePattern dv ->
                return $ SMT.bool $ Builtin.Bool.extractBoolDomainValue
                    "while translating dv to SMT.bool" dv
            NotPattern Not { notChild } ->
                -- \not is equivalent to BOOL.not for functional patterns.
                -- The following is safe because non-functional patterns
                -- will fail to translate.
                SMT.not <$> translateBool notChild
            ApplicationPattern app ->
                translateApplication (extract pat) app
            _ -> empty

    translateApplication
        ::  ( Given (MetadataTools Object StepperAttributes)
            , Ord (variable Object)
            , p ~ StepPattern Object variable
            )
        => Valid (variable Object) Object
        -> Application Object p
        -> Translator p SExpr
    translateApplication
        Valid { patternSort }
        Application { applicationChildren }
      = case getSmtlib of
            Nothing -> empty
            Just sExpr ->
                shortenSExpr
                <$> applySExpr sExpr
                <$> traverse translatePattern applicationChildren
        where
        getSmtlib
          | SortActualSort sortActual <- patternSort = do
            let SortActual { sortAttributes } = sortActual
                Attribute.Sort { smtlib } = sortAttributes
            Attribute.getSmtlib smtlib
          | otherwise = Nothing

    translatePattern
        :: StepPattern Object variable
        -> Translator (StepPattern Object variable) SExpr
    translatePattern pat =
        case getHook of
            Just builtinSort
              | builtinSort == Builtin.Bool.sort -> translateBool pat
              | builtinSort == Builtin.Int.sort -> translateInt pat
            _ -> case Recursive.project pat of
                    valid :< ApplicationPattern app ->
                        translateApplication valid app
                    _ -> empty
      where
        getHook = do
            Attribute.Sort { hook = Attribute.Hook h } <- getSortAttributes pat
            h

-- ----------------------------------------------------------------
-- Translator
type VarContext p = Map p (SExpr, SExpr)

type Translator p = MaybeT (StateT (VarContext p) (CounterT SMT))

evalTranslator :: Ord p => Translator p a -> MaybeT SMT a
evalTranslator = Morph.hoist (evalCounterT . flip evalStateT Map.empty)

runTranslator :: Ord p => Translator p a -> MaybeT SMT (a, VarContext p)
runTranslator = evalTranslator . includeState
  where includeState comp = do
            comp' <- comp
            state <- State.get
            pure (comp', state)
