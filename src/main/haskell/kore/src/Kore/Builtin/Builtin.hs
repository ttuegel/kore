{- |
Module      : Kore.Builtin.Builtin
Description : Built-in sort, symbol, and pattern verifiers
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Builtin as Builtin
@
 -}
module Kore.Builtin.Builtin
    (
      -- * Using builtin verifiers
      Verifiers (..)
    , SymbolVerifier, SymbolVerifiers
    , SortDeclVerifier, SortDeclVerifiers
    , SortVerifier
    , PatternVerifier (..)
    , Function
    , Parser
    , symbolVerifier
    , sortDeclVerifier
      -- * Declaring builtin verifiers
    , verifySortDecl
    , verifySort
    , verifySymbol
    , verifySymbolArguments
    , verifyDomainValue
    , verifyStringLiteral
    , parseDomainValue
    , parseString
    , parseStringLiteral
      -- * Implementing builtin functions
    , notImplemented
    , unaryOperator
    , binaryOperator
    , ternaryOperator
    , functionEvaluator
    , verifierBug
    , wrongArity
    , runParser
    , appliedFunction
    , lookupSymbol
    , expectNormalConcreteTerm
    , getAttemptedFunction
    ) where

import           Control.Monad
                 ( zipWithM_ )
import           Control.Monad.Except
                 ( ExceptT, MonadError, runExceptT )
import qualified Control.Monad.Except as Except
import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.HashMap.Strict
                 ( HashMap )
import qualified Data.HashMap.Strict as HashMap
import           Data.Semigroup
                 ( Semigroup (..) )
import           Data.Void
                 ( Void )
import           GHC.Stack
                 ( HasCallStack )
import           Text.Megaparsec
                 ( Parsec )
import qualified Text.Megaparsec as Parsec

import           Kore.AST.Common
                 ( Application (..), BuiltinDomain (..), CommonPurePattern,
                 ConcretePurePattern, DomainValue (..), Id (..), Pattern (..),
                 PureMLPattern, Sort (..), SortActual (..), SortVariable (..),
                 SymbolOrAlias (..), Variable )
import           Kore.AST.Kore
                 ( CommonKorePattern )
import           Kore.AST.MetaOrObject
                 ( Object )
import           Kore.AST.PureML
                 ( asConcretePurePattern )
import           Kore.AST.Sentence
                 ( KoreSentenceSort, KoreSentenceSymbol, SentenceSort (..),
                 SentenceSymbol (..) )
import           Kore.ASTUtils.SmartPatterns
                 ( pattern StringLiteral_ )
import           Kore.ASTVerifier.Verifier
import           Kore.Builtin.Hook
                 ( Hook (..) )
import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule
                 ( KoreIndexedModule, SortDescription )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..) )
import qualified Kore.IndexedModule.MetadataTools as MetadataTools
import qualified Kore.IndexedModule.Resolvers as IndexedModule
import qualified Kore.Proof.Value as Value
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern )
import           Kore.Step.Function.Data
                 ( ApplicationFunctionEvaluator (ApplicationFunctionEvaluator),
                 AttemptedFunction (..) )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Simplification.Data
                 ( CommonPureMLPatternSimplifier, SimplificationProof (..),
                 Simplifier )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes )

type Parser = Parsec Void String

type Function = ApplicationFunctionEvaluator Object Variable

-- | Verify a sort declaration.
type SortDeclVerifier =
       KoreSentenceSort Object
    -- ^ Sort declaration to verify
    -> Verifier ()

type SortVerifier =
       (Id Object -> Verifier (SortDescription Object))
    -- ^ Find a sort declaration
    -> Sort Object
    -- ^ Sort to verify
    -> Verifier ()

-- | @SortDeclVerifiers@ associates a @SortDeclVerifier@ with its builtin sort name.
type SortDeclVerifiers = HashMap String SortDeclVerifier

type SymbolVerifier =
       (Id Object -> Verifier (SortDescription Object))
    -- ^ Find a sort declaration
    -> KoreSentenceSymbol Object
    -- ^ Symbol declaration to verify
    -> Verifier ()

{- | @SymbolVerifiers@ associates a @SymbolVerifier@ with each builtin
  symbol name.
 -}
type SymbolVerifiers = HashMap String SymbolVerifier

newtype PatternVerifier =
    PatternVerifier
    {
      {- | Verify object-level patterns in builtin sorts.

        The first argument is a function to look up sorts.
        The second argument is a (projected) object-level pattern to verify.
        If the pattern is not in a builtin sort, the verifier should
        @return ()@.

        See also: 'verifyDomainValue'
      -}
      verifyPattern
          :: (Id Object -> Verifier (SortDescription Object))
          -> Pattern Object Variable CommonKorePattern
          -> Verifier (Pattern Object Variable CommonKorePattern)
    }

instance Semigroup PatternVerifier where
    {- | Conjunction of 'PatternVerifier's.

      The resulting @PatternVerifier@ succeeds when both constituents succeed.

     -}
    (<>) a b =
        PatternVerifier
        { verifyPattern = \findSort pat1 -> do
            pat2 <- verifyPattern a findSort pat1
            verifyPattern b findSort pat2
        }

instance Monoid PatternVerifier where
    {- | Trivial 'PatternVerifier' (always succeeds).
     -}
    mempty = PatternVerifier { verifyPattern = \_ pat -> return pat }
    mappend = (<>)

type DomainValueVerifier child =
    DomainValue Object child -> Verifier (DomainValue Object child)

{- | Verify builtin sorts, symbols, and patterns.
 -}
data Verifiers =
    Verifiers
    { sortDeclVerifiers :: SortDeclVerifiers
    , symbolVerifiers :: SymbolVerifiers
    , patternVerifier :: PatternVerifier
    }

{- | Look up and apply a builtin sort declaration verifier.

  The 'Hook' name should refer to a builtin sort; if it is unset or the name is
  not recognized, verification succeeds.

 -}
sortDeclVerifier :: Verifiers -> Hook -> SortDeclVerifier
sortDeclVerifier Verifiers { sortDeclVerifiers } hook =
    let
        hookedSortVerifier :: Maybe SortDeclVerifier
        hookedSortVerifier = do
            -- Get the builtin sort name.
            sortName <- getHook hook
            HashMap.lookup sortName sortDeclVerifiers
    in
        case hookedSortVerifier of
            Nothing ->
                -- There is nothing to verify because either
                -- 1. the sort is not hooked, or
                -- 2. there is no SortVerifier registered to the hooked name.
                -- In either case, there is nothing more to do.
                \_ -> pure ()
            Just verifier ->
                -- Invoke the verifier that is registered to this builtin sort.
                verifier

{- | Look up and apply a builtin symbol verifier.

  The 'Hook' name should refer to a builtin symbol; if it is unset or the name is
  not recognized, verification succeeds.

 -}
symbolVerifier :: Verifiers -> Hook -> SymbolVerifier
symbolVerifier Verifiers { symbolVerifiers } hook =
    let
        hookedSymbolVerifier :: Maybe SymbolVerifier
        hookedSymbolVerifier = do
            -- Get the builtin sort name.
            symbolName <- getHook hook
            HashMap.lookup symbolName symbolVerifiers
    in
        case hookedSymbolVerifier of
            Nothing ->
                -- There is nothing to verify because either
                -- 1. the symbol is not hooked, or
                -- 2. there is no SymbolVerifier registered to the hooked name.
                -- In either case, there is nothing more to do.
                \_ _ -> pure ()
            Just verifier ->
                -- Invoke the verifier that is registered to this builtin symbol.
                verifier

notImplemented :: Function
notImplemented =
    ApplicationFunctionEvaluator notImplemented0
  where
    notImplemented0 _ _ _ = pure (NotApplicable, SimplificationProof)

{- | Verify a builtin sort declaration.

  Check that the hooked sort does not take any sort parameters.

 -}
verifySortDecl :: SortDeclVerifier
verifySortDecl SentenceSort { sentenceSortParameters } =
    case sentenceSortParameters of
        [] -> pure ()
        _ ->
            Kore.Error.koreFail
                ("Expected 0 sort parameters, found "
                    ++ show (length sentenceSortParameters))

{- | Verify the occurrence of a builtin sort.

  Check that the sort is hooked to the named builtin. The sort parameters are
  already checked by the verifier.

 -}
verifySort
    :: (Id Object -> Verifier (SortDescription Object))
    -> String
    -> Sort Object
    -> Verifier ()
verifySort findSort builtinName (SortActualSort SortActual { sortActualName }) =
    do
        SentenceSort { sentenceSortAttributes } <- findSort sortActualName
        let expectHook = Hook (Just builtinName)
        declHook <- parseAttributes sentenceSortAttributes
        Kore.Error.koreFailWhen (expectHook /= declHook)
            ("Sort '" ++ getId sortActualName
                ++ "' is not hooked to builtin sort '"
                ++ builtinName ++ "'")
verifySort _ _ (SortVariableSort SortVariable { getSortVariable }) =
    Kore.Error.koreFail
        ("unexpected sort variable '" ++ getId getSortVariable ++ "'")

{- | Verify a builtin symbol declaration.

  The declared sorts must match the builtin sorts.

  See also: 'verifySymbolArguments'

 -}
verifySymbol
    :: SortVerifier  -- ^ Builtin result sort
    -> [SortVerifier]  -- ^ Builtin argument sorts
    -> SymbolVerifier
verifySymbol
    verifyResult
    verifyArguments
    findSort
    decl@SentenceSymbol { sentenceSymbolResultSort = result }
  =
    do
        Kore.Error.withContext "In result sort"
            (verifyResult findSort result)
        verifySymbolArguments verifyArguments findSort decl

{- | Verify the arguments of a builtin sort declaration.

  The declared argument sorts must match the builtin argument
  sorts. @verifySymbolArguments@ only checks the symbol's argument sorts; use
  'verifySymbol' if it is also necessary to check the symbol's result sort.

  See also: 'verifySymbol'

 -}
verifySymbolArguments
    :: [SortVerifier]  -- ^ Builtin argument sorts
    -> SymbolVerifier
verifySymbolArguments
    verifyArguments
    findSort
    SentenceSymbol { sentenceSymbolSorts = sorts }
  =
    Kore.Error.withContext "In argument sorts"
    (do
        Kore.Error.koreFailWhen (arity /= builtinArity)
            ("Expected " ++ show builtinArity
             ++ " arguments, found " ++ show arity)
        zipWithM_ (\verify sort -> verify findSort sort) verifyArguments sorts
    )
  where
    builtinArity = length verifyArguments
    arity = length sorts

{- | Verify a domain value pattern.

  If the given pattern is not a domain value, it is skipped.

  See also: 'verifyStringLiteral'

 -}
verifyDomainValue
    :: String  -- ^ Builtin sort name
    -> DomainValueVerifier (BuiltinDomain CommonKorePattern)
    -- ^ validation function
    -> PatternVerifier
verifyDomainValue builtinSort validate =
    PatternVerifier { verifyPattern }
  where
    verifyPattern
        :: (Id Object -> Verifier (SortDescription Object))
        -- ^ Function to lookup sorts by identifier
        -> Pattern Object Variable CommonKorePattern
        -- ^ Pattern to verify
        -> Verifier (Pattern Object Variable CommonKorePattern)
    verifyPattern findSort pat =
        case pat of
            DomainValuePattern dv@DomainValue { domainValueSort } ->
                Kore.Error.withContext
                    ("Verifying domain value in builtin sort '"
                        ++ builtinSort ++ "'")
                    (skipOtherSorts pat domainValueSort
                        (DomainValuePattern <$> validate dv)
                    )
            _ -> return pat  -- no domain value to verify
      where
        -- | Run @next@ if @sort@ is hooked to @builtinSort@; do nothing
        -- otherwise.
        skipOtherSorts
            :: a
            -- ^ default value
            -> Sort Object
            -- ^ Sort of pattern under verification
            -> Verifier a
            -- ^ Verifier run iff pattern sort is hooked to designated builtin
            -> Verifier a
        skipOtherSorts a sort next = do
            decl <-
                Except.catchError
                    (Just <$> verifySort findSort builtinSort sort)
                    (\_ -> return Nothing)
            case decl of
              Nothing -> return a
              Just () -> next

{- | Verify a literal string domain value.

    It is an error if the domain value pattern is not a literal string, but if
    the domain value has already been decoded into 'BuiltinDomain', it is
    skipped.

    See also: 'verifyDomainValue'

 -}
verifyStringLiteral
    :: (String -> Verifier (BuiltinDomain child))
    -- ^ validation function
    -> DomainValueVerifier (BuiltinDomain child)
verifyStringLiteral validate dv@DomainValue { domainValueChild } =
    case domainValueChild of
        BuiltinDomainPattern pat ->
            case pat of
                StringLiteral_ lit ->
                    do
                        builtin <- validate lit
                        return dv { domainValueChild = builtin }
                _ -> koreFail "Expected string literal"
        _ -> return dv

{- | Run a parser in a domain value pattern.

  An error is thrown if the domain value does not contain a literal string.
  The parsed value is returned.

 -}
parseDomainValue
    :: Parser a
    -> DomainValue Object (BuiltinDomain child)
    -> Verifier a
parseDomainValue
    parser
    DomainValue { domainValueChild }
  =
    Kore.Error.withContext "Parsing domain value"
        (case domainValueChild of
            BuiltinDomainPattern (StringLiteral_ lit) ->
                parseString parser lit
            _ -> Kore.Error.koreFail "Expected literal string"
        )

{- | Run a parser on a string.

 -}
parseString
    :: Parser a
    -> String
    -> Verifier a
parseString parser lit =
    let parsed = Parsec.parse (parser <* Parsec.eof) "<string literal>" lit
    in castParseError parsed
  where
    castParseError =
        either (Kore.Error.koreFail . Parsec.parseErrorPretty) pure

{- | Run a parser in a domain value pattern.

  An error is thrown if the domain value does not contain a literal string.
  The parsed value is returned.

 -}
parseStringLiteral
    :: Parser a
    -> String
    -> Verifier a
parseStringLiteral
    parser
    lit
  =
    Kore.Error.withContext "Parsing string literal"
        (
            let parsed =
                    Parsec.parse
                        (parser <* Parsec.eof)
                        "<string literal>"
                        lit
            in castParseError parsed
        )
  where
    castParseError =
        either (Kore.Error.koreFail . Parsec.parseErrorPretty) pure

{- | Return the supplied pattern as an 'AttemptedFunction'.

  No substitution or predicate is applied.

  See also: 'ExpandedPattern'
 -}
appliedFunction
    :: Monad m
    => CommonExpandedPattern Object
    -> m (AttemptedFunction Object Variable)
appliedFunction epat =
    (return . Applied . OrOfExpandedPattern.make) [epat]

{- | Construct a builtin unary operator.

  The operand type may differ from the result type.

  The function is skipped if its arguments are not domain values.
  It is an error if the wrong number of arguments is given; this must be checked
  during verification.

 -}
unaryOperator
    :: Parser a
    -- ^ Parse operand
    -> (Sort Object -> b -> CommonExpandedPattern Object)
    -- ^ Render result as pattern with given sort
    -> String
    -- ^ Builtin function name (for error messages)
    -> (a -> b)
    -- ^ Operation on builtin types
    -> Function
unaryOperator
    parser
    asPattern
    ctx
    op
  =
    functionEvaluator unaryOperator0
  where
    get = runParser ctx . parseDomainValue parser
    unaryOperator0 _ _ resultSort children =
        case Functor.Foldable.project <$> children of
            [DomainValuePattern a] -> do
                -- Apply the operator to a domain value
                let r = op (get a)
                (appliedFunction . asPattern resultSort) r
            [_] -> return NotApplicable
            _ -> wrongArity ctx

{- | Construct a builtin binary operator.

  Both operands have the same builtin type, which may be different from the
  result type.

  The function is skipped if its arguments are not domain values.
  It is an error if the wrong number of arguments is given; this must be checked
  during verification.

 -}
binaryOperator
    :: Parser a
    -- ^ Parse operand
    -> (Sort Object -> b -> CommonExpandedPattern Object)
    -- ^ Render result as pattern with given sort
    -> String
    -- ^ Builtin function name (for error messages)
    -> (a -> a -> b)
    -- ^ Operation on builtin types
    -> Function
binaryOperator
    parser
    asPattern
    ctx
    op
  =
    functionEvaluator binaryOperator0
  where
    get = runParser ctx . parseDomainValue parser
    binaryOperator0 _ _ resultSort children =
        case Functor.Foldable.project <$> children of
            [DomainValuePattern a, DomainValuePattern b] -> do
                -- Apply the operator to two domain values
                let r = op (get a) (get b)
                (appliedFunction . asPattern resultSort) r
            [_, _] -> return NotApplicable
            _ -> wrongArity ctx

{- | Construct a builtin ternary operator.

  All three operands have the same builtin type, which may be different from the
  result type.

  The function is skipped if its arguments are not domain values.
  It is an error if the wrong number of arguments is given; this must be checked
  during verification.

 -}
ternaryOperator
    :: Parser a
    -- ^ Parse operand
    -> (Sort Object -> b -> CommonExpandedPattern Object)
    -- ^ Render result as pattern with given sort
    -> String
    -- ^ Builtin function name (for error messages)
    -> (a -> a -> a -> b)
    -- ^ Operation on builtin types
    -> Function
ternaryOperator
    parser
    asPattern
    ctx
    op
  =
    functionEvaluator ternaryOperator0
  where
    get = runParser ctx . parseDomainValue parser
    ternaryOperator0 _ _ resultSort children =
        case Functor.Foldable.project <$> children of
            [DomainValuePattern a, DomainValuePattern b, DomainValuePattern c] -> do
                -- Apply the operator to three domain values
                let r = op (get a) (get b) (get c)
                (appliedFunction . asPattern resultSort) r
            [_, _, _] -> return NotApplicable
            _ -> wrongArity ctx

functionEvaluator
    :: (  MetadataTools Object StepperAttributes
       -> CommonPureMLPatternSimplifier Object
       -> Sort Object
       -> [CommonPurePattern Object]
       -> Simplifier (AttemptedFunction Object Variable)
       )
    -- ^ Builtin function implementation
    -> Function
functionEvaluator impl =
    ApplicationFunctionEvaluator evaluator
  where
    evaluator
        tools
        simplifier
        Application
            { applicationSymbolOrAlias =
                (MetadataTools.getResultSort tools -> resultSort)
            , applicationChildren
            }
      =
        do
            attempt <- impl tools simplifier resultSort applicationChildren
            return (attempt, SimplificationProof)

{- | Abort due to an internal error that should be prevented by the verifier.

    Such an error is a bug in Kore that we would like the user to report.

 -}
verifierBug :: HasCallStack => String -> a
verifierBug msg =
    (error . unlines)
        [ "Internal error: " ++ msg
        , "This error should be prevented by the verifier."
        , "Please report this as a bug."
        ]

{- | Evaluation failure due to a builtin call with the wrong arity.

 -}
wrongArity :: HasCallStack => String -> a
wrongArity ctx = verifierBug (ctx ++ ": Wrong number of arguments")

{- | Run a parser on a verified domain value.

    Any parse failure indicates a bug in the well-formedness checker; in this
    case an error is thrown.

 -}
runParser :: HasCallStack => String -> Verifier a -> a
runParser ctx verifier =
    case runVerifier verifier of
        Left e -> verifierBug (ctx ++ ": " ++ Kore.Error.printError e)
        Right a -> a

{- | Look up the symbol hooked to the named builtin in the provided module.
 -}
lookupSymbol
    :: String
    -- ^ builtin name
    -> KoreIndexedModule attrs
    -> Either (Error e) (SymbolOrAlias Object)
lookupSymbol builtinName indexedModule
  = do
    symbolOrAliasConstructor <-
        IndexedModule.resolveHook indexedModule builtinName
    _ <- IndexedModule.resolveSymbol indexedModule symbolOrAliasConstructor
    return SymbolOrAlias
        { symbolOrAliasConstructor
        , symbolOrAliasParams = []
        }

{- | Ensure that a 'PureMLPattern' is a concrete, normalized term.

    If the pattern is not concrete and normalized, the function is
    'NotApplicable'.

 -}
expectNormalConcreteTerm
    :: MonadError (AttemptedFunction Object Variable) m
    => MetadataTools level StepperAttributes
    -> PureMLPattern level variable
    -> m (ConcretePurePattern level)
expectNormalConcreteTerm tools purePattern =
    maybe (Except.throwError NotApplicable) return $ do
        p <- asConcretePurePattern purePattern
        -- TODO (thomas.tuegel): Use the return value as the term. Will require
        -- factoring BuiltinDomain out of Kore.AST.Common.
        _ <- Value.fromConcretePurePattern tools p
        return p

{- | Run a computation which can return early.

    See also: 'expectNormalConcreteTerm'

 -}
getAttemptedFunction :: Monad m => ExceptT r m r -> m r
getAttemptedFunction = fmap (either id id) . runExceptT
