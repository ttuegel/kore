module Test.Kore.Builtin.Builtin where

import Test.Tasty
       ( TestTree )
import Test.Tasty.HUnit
       ( assertEqual )

import           Control.Concurrent.MVar
                 ( MVar )
import qualified Control.Lens as Lens
import           Control.Monad.Except
                 ( runExceptT )
import           Control.Monad.Reader
                 ( runReaderT )
import           Data.Function
                 ( (&) )
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Proxy
import           Data.These
                 ( These (This) )

import           Kore.AST.Pure
import           Kore.AST.Sentence
import qualified Kore.ASTUtils.SmartConstructors as Kore
import           Kore.ASTUtils.SmartPatterns
import           Kore.ASTVerifier.DefinitionVerifier
import           Kore.ASTVerifier.Error
                 ( VerifyError )
import qualified Kore.Builtin as Builtin
import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools (..), SymbolOrAliasSorts,
                 extractMetadataTools )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.AxiomPatterns
                 ( RewriteRule )
import           Kore.Step.BaseStep
                 ( StepProof, StepResult (StepResult), stepWithRule )
import qualified Kore.Step.BaseStep as StepResult
                 ( StepResult (..) )
import           Kore.Step.Error
                 ( StepError )
import           Kore.Step.ExpandedPattern
                 ( CommonExpandedPattern, Predicated (..) )
import           Kore.Step.Function.Data
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
import           Kore.Step.Pattern
import           Kore.Step.Simplification.Data
import qualified Kore.Step.Simplification.Pattern as Pattern
import qualified Kore.Step.Simplification.PredicateSubstitution as PredicateSubstitution
import           Kore.Step.StepperAttributes
import           SMT
                 ( MonadSMT (..), SMT, Solver )
import qualified SMT

import           Test.Kore.Builtin.Definition
import qualified Test.Kore.Step.MockSimplifiers as Mock
import           Test.SMT

mkPair
    :: Sort Object
    -> Sort Object
    -> CommonStepPattern Object
    -> CommonStepPattern Object
    -> CommonStepPattern Object
mkPair lSort rSort l r = App_ (pairSymbol lSort rSort) [l, r]

substitutionSimplifier
    :: MetadataTools level StepperAttributes
    -> PredicateSubstitutionSimplifier level Simplifier
substitutionSimplifier tools =
    PredicateSubstitution.create
        tools
        (StepPatternSimplifier
            (\_ p ->
                return
                    ( OrOfExpandedPattern.make
                        [ Predicated
                            { term = Kore.mkTop
                            , predicate = Predicate.wrapPredicate p
                            , substitution = mempty
                            }
                        ]
                    , SimplificationProof
                    )
            )
        )

-- | 'testSymbol' is useful for writing unit tests for symbols.
testSymbolWithSolver
    ::  ( p ~ CommonStepPattern Object
        , expanded ~ CommonExpandedPattern Object
        )
    => (p -> SMT expanded)
    -- ^ evaluator function for the builtin
    -> String
    -- ^ test name
    -> SymbolOrAlias Object
    -- ^ symbol being tested
    -> [p]
    -- ^ arguments for symbol
    -> expanded
    -- ^ expected result
    -> TestTree
testSymbolWithSolver eval title symbol args expected =
    testCaseWithSolver title $ \solver -> do
        actual <- runReaderT (SMT.getSMT $ eval $ App_ symbol args) solver
        assertEqual "" expected actual

-- -------------------------------------------------------------
-- * Evaluation

verify
    :: KoreDefinition
    -> Either
        (Kore.Error.Error VerifyError)
        (Map ModuleName (KoreIndexedModule StepperAttributes))
verify = verifyAndIndexDefinition attrVerify Builtin.koreVerifiers
  where
    attrVerify = defaultAttributesVerification Proxy

-- TODO (traiansf): Get rid of this.
-- The function below works around several limitations of
-- the current tool by tricking the tool into believing that
-- functions are constructors (so that function patterns can match)
-- and that @kseq@ and @dotk@ are both functional and constructor.
constructorFunctions
    :: KoreIndexedModule StepperAttributes
    -> KoreIndexedModule StepperAttributes
constructorFunctions ixm =
    ixm
        { indexedModuleObjectSymbolSentences =
            Map.mapWithKey
                constructorFunctions1
                (indexedModuleObjectSymbolSentences ixm)
        , indexedModuleObjectAliasSentences =
            Map.mapWithKey
                constructorFunctions1
                (indexedModuleObjectAliasSentences ixm)
        , indexedModuleImports = recurseIntoImports <$> indexedModuleImports ixm
        }
  where
    constructorFunctions1 ident (atts, defn) =
        ( atts
            & lensConstructor Lens.<>~ Constructor isCons
            & lensFunctional Lens.<>~ Functional (isCons || isInj)
            & lensInjective Lens.<>~ Injective (isCons || isInj)
            & lensSortInjection Lens.<>~ SortInjection isInj
        , defn
        )
      where
        isInj = getId ident == "inj"
        isCons = elem (getId ident) ["kseq", "dotk"]

    recurseIntoImports (attrs, attributes, importedModule) =
        (attrs, attributes, constructorFunctions importedModule)

indexedModules :: Map ModuleName (KoreIndexedModule StepperAttributes)
indexedModules =
    either (error . Kore.Error.printError) id (verify testDefinition)

indexedModule :: KoreIndexedModule StepperAttributes
Just indexedModule = Map.lookup testModuleName indexedModules

testMetadataTools :: MetadataTools Object StepperAttributes
testMetadataTools = extractMetadataTools (constructorFunctions indexedModule)

testSymbolOrAliasSorts :: SymbolOrAliasSorts Object
MetadataTools { symbolOrAliasSorts = testSymbolOrAliasSorts} = testMetadataTools

testSubstitutionSimplifier :: PredicateSubstitutionSimplifier Object Simplifier
testSubstitutionSimplifier = Mock.substitutionSimplifier testMetadataTools

evaluators :: BuiltinAndAxiomsFunctionEvaluatorMap Object
evaluators = Map.map This $ Builtin.koreEvaluators indexedModule

evaluate
    :: MonadSMT m
    => CommonStepPattern Object
    -> m (CommonExpandedPattern Object)
evaluate =
    (<$>) fst
    . liftSMT
    . evalSimplifier
    . Pattern.simplify
        testMetadataTools
        testSubstitutionSimplifier
        evaluators

evaluateWith
    :: MVar Solver
    -> CommonStepPattern Object
    -> IO (CommonExpandedPattern Object)
evaluateWith solver patt =
    runReaderT (SMT.getSMT $ evaluate patt) solver

runStep
    :: CommonExpandedPattern Object
    -- ^ configuration
    -> RewriteRule Object
    -- ^ axiom
    -> IO
        (Either
            (StepError Object Variable)
            (CommonExpandedPattern Object, StepProof Object Variable)
        )
runStep configuration axiom = do
    ioResult <- runStepResult configuration axiom
    return $ do
        (StepResult { rewrittenPattern }, proof) <- ioResult
        return (rewrittenPattern, proof)

runStepResult
    :: CommonExpandedPattern Object
    -- ^ configuration
    -> RewriteRule Object
    -- ^ axiom
    -> IO
        (Either
            (StepError Object Variable)
            (StepResult Object Variable, StepProof Object Variable)
        )
runStepResult configuration axiom =
    (runSMT . evalSimplifier . runExceptT)
        (stepWithRule
            testMetadataTools
            testSubstitutionSimplifier
            configuration
            axiom
        )

runSMT :: SMT a -> IO a
runSMT = SMT.runSMT SMT.defaultConfig

runStepWith
    :: MVar Solver
    -> CommonExpandedPattern Object
    -- ^ configuration
    -> RewriteRule Object
    -- ^ axiom
    -> IO
        (Either
            (StepError Object Variable)
            (CommonExpandedPattern Object, StepProof Object Variable)
        )
runStepWith solver configuration axiom = do
    ioResult <- runStepResultWith
        solver configuration axiom
    return $ do
        (StepResult { rewrittenPattern }, proof) <- ioResult
        return (rewrittenPattern, proof)

runStepResultWith
    :: MVar Solver
    -> CommonExpandedPattern Object
    -- ^ configuration
    -> RewriteRule Object
    -- ^ axiom
    -> IO
        (Either
            (StepError Object Variable)
            (StepResult Object Variable, StepProof Object Variable)
        )
runStepResultWith solver configuration axiom =
    let smt =
            (evalSimplifier . runExceptT)
                (stepWithRule
                    testMetadataTools
                    testSubstitutionSimplifier
                    configuration
                    axiom
                )
    in runReaderT (SMT.getSMT smt) solver
