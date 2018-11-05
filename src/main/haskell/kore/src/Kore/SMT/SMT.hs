{-|
Module      : Kore.SMT.SMT
Description : Basic SMT interface.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}

module Kore.SMT.SMT
    ( unsafeTryRefutePredicate
    , translatePredicate
    ) where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Error
                 ( MaybeT, runMaybeT )
import           Control.Monad.Except
import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.Proxy
import           Data.Reflection
                 ( Given (..) )
import           Data.SBV as SMT
import qualified Data.Text as Text
import           GHC.IO.Unsafe
import           Prelude hiding
                 ( and, not, or )

import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import qualified Kore.Builtin.Bool as Builtin.Bool
import qualified Kore.Builtin.Builtin as Builtin
import           Kore.Builtin.Hook
import qualified Kore.Builtin.Int as Builtin.Int
import           Kore.IndexedModule.MetadataTools
import qualified Kore.Predicate.Predicate as Kore
import           Kore.SMT.Config
import           Kore.SMT.Translator
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )
import qualified Kore.Step.StepperAttributes as StepperAttributes

config :: SMTConfig
config = z3

{- | Attempt to disprove the given predicate using SMT.

 -}
unsafeTryRefutePredicate
    :: forall level variable .
       ( Given (MetadataTools level StepperAttributes)
       , MetaOrObject level
       , Ord (variable level)
       , Show (variable level)
       , SortedVariable variable
       )
    => SMTTimeOut
    -> Kore.Predicate level variable
    -> Maybe Bool
{-# NOINLINE unsafeTryRefutePredicate #-} -- Needed by: unsafePerformIO
unsafeTryRefutePredicate (SMTTimeOut timeout) p =
  case isMetaOrObject (Proxy :: Proxy level) of
    IsMeta   -> Nothing
    IsObject ->
        unsafePerformIO $ do
            let smtPredicate = do
                    setTimeOut timeout
                    smt <- runMaybeT (translatePredicate p)
                    case smt of
                        Nothing -> sBool "TranslationFailed"
                        Just p' -> return $ bnot p'
            res <- proveWith config smtPredicate
            return $ case res of
                ThmResult (Satisfiable   _ _) -> Nothing
                ThmResult (Unsatisfiable _ _) -> Just False
                _ -> Nothing

{- | Translate a predicate for SMT.

The predicate may inhabit an arbitrary sort. Logical connectives are translated
to their SMT counterparts. Quantifiers, @\\ceil@, @\\floor@, and @\\in@ are
uninterpreted (translated as variables) as is @\\equals@ if its arguments are
not builtins or predicates. All other patterns are not translated and prevent
the predicate from being sent to SMT.

 -}
translatePredicate
    :: forall variable.
        (Ord (variable Object), Given (MetadataTools Object StepperAttributes))
    => Kore.Predicate Object variable
    -> MaybeT Symbolic SBool
translatePredicate predicate =
    runTranslator
        (translatePredicatePattern $ Kore.unwrapPredicate predicate)
  where
    translatePredicatePattern
        :: PureMLPattern Object variable
        -> Translator (PureMLPattern Object variable) SBool
    translatePredicatePattern pat =
        case Functor.Foldable.project pat of
            -- Logical connectives: translate as connectives
            AndPattern and -> translatePredicateAnd and
            BottomPattern _ -> return SMT.false
            EqualsPattern eq ->
                -- Equality of predicates and builtins can be translated to
                -- equality in the SMT solver, but other patterns must remain
                -- uninterpreted.
                translatePredicateEquals eq <|> translateUninterpretedBool pat
            IffPattern iff -> translatePredicateIff iff
            ImpliesPattern implies -> translatePredicateImplies implies
            NotPattern not -> translatePredicateNot not
            OrPattern or -> translatePredicateOr or
            TopPattern _ -> return SMT.true

            -- Uninterpreted: translate as variables
            CeilPattern _ -> translateUninterpretedBool pat
            ExistsPattern _ -> translateUninterpretedBool pat
            FloorPattern _ -> translateUninterpretedBool pat
            ForallPattern _ -> translateUninterpretedBool pat
            InPattern _ -> translateUninterpretedBool pat

            -- Invalid: no translation, should not occur in predicates
            ApplicationPattern _ -> empty
            DomainValuePattern _ -> empty
            NextPattern _ -> empty
            RewritesPattern _ -> empty
            VariablePattern _ -> empty

    hookTools :: MetadataTools Object Hook
    hookTools = StepperAttributes.hook <$> given

    translatePredicateAnd And { andFirst, andSecond } =
        (SMT.&&&)
            <$> translatePredicatePattern andFirst
            <*> translatePredicatePattern andSecond

    translatePredicateEquals
        Equals
            { equalsOperandSort
            , equalsResultSort
            , equalsFirst
            , equalsSecond
            }
      | equalsOperandSort == equalsResultSort =
        -- Child patterns are predicates.
        translatePredicateEqualsPredicate
      | otherwise =
        case getHook (sortAttributes hookTools equalsOperandSort) of
            Nothing -> empty
            Just builtinSort ->
                -- Child patterns are hooked to builtins that may be
                -- translatable to SMT theories.
                translatePredicateEqualsBuiltin builtinSort
      where
        translatePredicateEqualsPredicate =
            (SMT.<=>)
                <$> translatePredicatePattern equalsFirst
                <*> translatePredicatePattern equalsSecond
        translatePredicateEqualsBuiltin builtinSort
          | builtinSort == Builtin.Bool.sort = translateEqualsBool
          | builtinSort == Builtin.Int.sort = translateEqualsInt
          | otherwise = empty
        translateEqualsInt =
            (SMT..==)
                <$> translateInt equalsFirst
                <*> translateInt equalsSecond
        translateEqualsBool =
            (SMT..==)
                <$> translateBool equalsFirst
                <*> translateBool equalsSecond

    translatePredicateIff Iff { iffFirst, iffSecond } =
        (SMT.<=>)
            <$> translatePredicatePattern iffFirst
            <*> translatePredicatePattern iffSecond

    translatePredicateImplies Implies { impliesFirst, impliesSecond } =
        (SMT.==>)
            <$> translatePredicatePattern impliesFirst
            <*> translatePredicatePattern impliesSecond

    translatePredicateNot Not { notChild } =
        SMT.bnot <$> translatePredicatePattern notChild

    translatePredicateOr Or { orFirst, orSecond } =
        (SMT.|||)
            <$> translatePredicatePattern orFirst
            <*> translatePredicatePattern orSecond

-- | Translate a functional pattern in the builtin Int sort for SMT.
translateInt
    :: forall p variable.
        ( Given (MetadataTools Object StepperAttributes)
        , Ord (variable Object)
        , p ~ PureMLPattern Object variable
        )
    => p
    -> Translator p SInteger
translateInt pat =
    case Functor.Foldable.project pat of
        VariablePattern _ -> translateUninterpretedInt pat
        DomainValuePattern dv ->
            (return . SMT.literal . Builtin.runParser ctx)
                (Builtin.parseDomainValue Builtin.Int.parse dv)
          where
            ctx = Text.unpack Builtin.Int.sort
        ApplicationPattern app ->
            translateApplication app
        _ -> empty
  where
    hookTools :: MetadataTools Object Hook
    hookTools = StepperAttributes.hook <$> given

    translateApplication
        Application
            { applicationSymbolOrAlias
            , applicationChildren
            }
      =
        case getHook (symAttributes hookTools applicationSymbolOrAlias) of
            Nothing -> empty
            Just hook ->
                case hook of
                    "INT.min" -> binaryOp SMT.smin
                    "INT.max" -> binaryOp SMT.smax
                    "INT.add" -> binaryOp (+)
                    "INT.sub" -> binaryOp (-)
                    "INT.mul" -> binaryOp (*)
                    "INT.tdiv" -> binaryOp sQuot
                    "INT.tmod" -> binaryOp sRem
                    "INT.and" -> binaryOp (.&.)
                    "INT.or" -> binaryOp (.|.)
                    "INT.xor" -> binaryOp xor
                    "INT.not" -> unaryOp complement
                    _ -> empty
              where
                ctx = Text.unpack hook
                unaryOp op =
                    case applicationChildren of
                        [first] ->
                            op <$> translateInt first
                        _ ->
                            Builtin.wrongArity ctx
                binaryOp op =
                    case applicationChildren of
                        [first, second] ->
                            op <$> translateInt first <*> translateInt second
                        _ ->
                            Builtin.wrongArity ctx

-- | Translate a functional pattern in the builtin Bool sort for SMT.
translateBool
    :: forall p variable.
        ( Given (MetadataTools Object StepperAttributes)
        , Ord (variable Object)
        , p ~ PureMLPattern Object variable
        )
    => p
    -> Translator p SBool
translateBool pat =
    case Functor.Foldable.project pat of
        VariablePattern _ -> translateUninterpretedBool pat
        DomainValuePattern dv ->
            (return . SMT.literal . Builtin.runParser ctx)
            (Builtin.parseDomainValue Builtin.Bool.parse dv)
            where
            ctx = Text.unpack Builtin.Bool.sort
        NotPattern Not { notChild } ->
            -- \not is equivalent to BOOL.not for functional patterns.
            -- The following is safe because non-functional patterns will fail
            -- to translate.
            SMT.bnot <$> translateBool notChild
        ApplicationPattern app ->
            translateApplication app
        _ -> empty
  where
    hookTools :: MetadataTools Object Hook
    hookTools = StepperAttributes.hook <$> given

    translateApplication
        Application
            { applicationSymbolOrAlias
            , applicationChildren
            }
      =
        case getHook (symAttributes hookTools applicationSymbolOrAlias) of
            Nothing -> empty
            Just hook ->
                case hook of
                    "INT.gt" -> binaryIntOp (SMT..>)
                    "INT.ge" -> binaryIntOp (SMT..>=)
                    "INT.eq" -> binaryIntOp (SMT..==)
                    "INT.le" -> binaryIntOp (SMT..<=)
                    "INT.lt" -> binaryIntOp (SMT..<)
                    "INT.ne" -> binaryIntOp (SMT../=)

                    "BOOL.or" -> binaryOp (SMT.|||)
                    "BOOL.and" -> binaryOp (SMT.&&&)
                    "BOOL.xor" -> binaryOp (SMT.<+>)
                    "BOOL.ne" -> binaryOp (SMT../=)
                    "BOOL.eq" -> binaryOp (SMT.<=>)
                    "BOOL.not" -> unaryOp SMT.bnot
                    "BOOL.implies" -> binaryOp (SMT.==>)
                    _ -> empty
              where
                ctx = Text.unpack hook
                unaryOp op =
                    case applicationChildren of
                        [first] ->
                            op <$> translateBool first
                        _ ->
                            Builtin.wrongArity ctx
                binaryOp op =
                    case applicationChildren of
                        [first, second] ->
                            op <$> translateBool first <*> translateBool second
                        _ ->
                            Builtin.wrongArity ctx
                binaryIntOp op =
                    case applicationChildren of
                        [first, second] ->
                            op <$> translateInt first <*> translateInt second
                        _ ->
                            Builtin.wrongArity ctx
