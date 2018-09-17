{-|
Module      : Kore.ASTVerifier.PatternVerifier
Description : Tools for verifying the wellformedness of a Kore 'Pattern'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.PatternVerifier
    ( verifyPattern
    , verifyProjectedPattern
    , verifyAliasLeftPattern
    , verifyAliasRightPattern
    , verifyStandalonePattern
    , verifyHelpers
    , VerifyHelpers
    ) where

import           Control.Lens
                 ( Lens', Traversal' )
import qualified Control.Lens as Lens
import           Control.Monad
                 ( foldM, zipWithM )
import           Control.Monad.Except
                 ( catchError, throwError )
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Map as Map
import           Data.Maybe
                 ( fromMaybe )
import qualified Data.Set as Set
import           Data.Text.Prettyprint.Doc
                 ( (<+>) )
import qualified Data.Text.Prettyprint.Doc as Pretty
import           Data.Text.Prettyprint.Doc.Render.String
                 ( renderString )

import           Kore.AST.Common
import           Kore.AST.Error
import           Kore.AST.Kore
import qualified Kore.AST.Lens as Lens
import           Kore.AST.MetaOrObject
import           Kore.AST.Sentence
import           Kore.ASTHelpers
import           Kore.ASTVerifier.SortVerifier
import           Kore.ASTVerifier.Verifier
import qualified Kore.Builtin as Builtin
import           Kore.Implicit.ImplicitSorts
import           Kore.IndexedModule.IndexedModule
import           Kore.IndexedModule.Resolvers
import           Kore.Unparser
import           Kore.Variables.Free
                 ( freeVariables )

data DeclaredVariables = DeclaredVariables
    { objectDeclaredVariables :: !(Map.Map (Id Object) (Variable Object))
    , metaDeclaredVariables   :: !(Map.Map (Id Meta) (Variable Meta))
    }

emptyDeclaredVariables :: DeclaredVariables
emptyDeclaredVariables = DeclaredVariables
    { objectDeclaredVariables = Map.empty
    , metaDeclaredVariables = Map.empty
    }

data VerifyHelpers level = VerifyHelpers
    { verifyHelpersFindSort
        :: !(Id level -> Verifier (SortDescription level))
    , verifyHelpersLookupAliasDeclaration
        :: !(Id level -> Verifier (KoreSentenceAlias level))
    , verifyHelpersLookupSymbolDeclaration
        :: !(Id level -> Verifier (KoreSentenceSymbol level))
    , verifyHelpersFindDeclaredVariables
        :: !(Id level -> Maybe (Variable level))
    }

metaVerifyHelpers
    :: KoreIndexedModule atts -> DeclaredVariables -> VerifyHelpers Meta
metaVerifyHelpers indexedModule declaredVariables =
    VerifyHelpers
        { verifyHelpersFindSort =
            fmap getIndexedSentence . resolveSort indexedModule
        , verifyHelpersLookupAliasDeclaration =
            fmap getIndexedSentence . resolveAlias indexedModule
        , verifyHelpersLookupSymbolDeclaration =
            fmap getIndexedSentence . resolveSymbol indexedModule
        , verifyHelpersFindDeclaredVariables =
            flip Map.lookup (metaDeclaredVariables declaredVariables)
        }

objectVerifyHelpers
    :: KoreIndexedModule atts -> DeclaredVariables -> VerifyHelpers Object
objectVerifyHelpers indexedModule declaredVariables =
    VerifyHelpers
        { verifyHelpersFindSort =
            fmap getIndexedSentence . resolveSort indexedModule
        , verifyHelpersLookupAliasDeclaration =
            fmap getIndexedSentence . resolveAlias indexedModule
        , verifyHelpersLookupSymbolDeclaration =
            fmap getIndexedSentence . resolveSymbol indexedModule
        , verifyHelpersFindDeclaredVariables =
            flip Map.lookup (objectDeclaredVariables declaredVariables)
        }

verifyHelpers
    :: MetaOrObject level
    => KoreIndexedModule attrs -> DeclaredVariables -> VerifyHelpers level
verifyHelpers indexedModule declaredVariables =
    let result =
            case isMetaOrObject result of
                IsMeta -> metaVerifyHelpers indexedModule declaredVariables
                IsObject -> objectVerifyHelpers indexedModule declaredVariables
    in result

addDeclaredVariable
    :: Unified Variable -> DeclaredVariables -> DeclaredVariables
addDeclaredVariable
    (UnifiedMeta variable@Variable { variableName })
    variables@DeclaredVariables{ metaDeclaredVariables = variablesDict }
  =
    variables
        { metaDeclaredVariables =
            Map.insert variableName variable variablesDict
        }
addDeclaredVariable
    (UnifiedObject variable@Variable { variableName })
    variables@DeclaredVariables{ objectDeclaredVariables = variablesDict }
  =
    variables
        { objectDeclaredVariables =
            Map.insert variableName variable variablesDict
        }

verifyAliasLeftPattern
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> Pattern level Variable CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyAliasLeftPattern
    builtinVerifier
    indexedModule
    declaredSortVariables
    pat
  =
    -- TODO: check that the left pattern is the alias symbol applied to
    -- non-repeating variables
    do
        freeVariables' <- verifyFreeVariables (asKorePattern pat)
        withPatternContext pat
            (verifyProjectedPattern
                builtinVerifier
                indexedModule
                (verifyHelpers indexedModule freeVariables')
                declaredSortVariables
                freeVariables'
                pat
            )

verifyAliasRightPattern
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> Pattern level Variable CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyAliasRightPattern
    builtinVerifier
    indexedModule
    declaredSortVariables
    pat
  =
    do
        freeVariables' <- verifyFreeVariables (asKorePattern pat)
        withPatternContext pat
            (verifyProjectedPattern
                builtinVerifier
                indexedModule
                (verifyHelpers indexedModule freeVariables')
                declaredSortVariables
                freeVariables'
                pat
            )

verifyStandalonePattern
    :: Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> CommonKorePattern
    -> Verifier (UnifiedSort, CommonKorePattern)
verifyStandalonePattern builtinVerifier indexedModule =
    verifyPattern builtinVerifier indexedModule Set.empty

{- | Verify that a 'CommonKorePattern' is well-formed Kore pattern.

    The pattern is updated during verification so that:

    - Domain values belonging to builtin domains are decoded from strings to the
      internal representation.

    Free variables in the pattern are verified and considered implicitly
    declared.

 -}
verifyPattern
    :: Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -- ^ The module containing all definitions which are visible in this
    -- pattern.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables which are visible in this pattern.
    -> CommonKorePattern
    -> Verifier (UnifiedSort, CommonKorePattern)
verifyPattern
    builtinVerifier
    indexedModule
    sortVariables
    unifiedPattern
  = do
    freeVariables1 <- verifyFreeVariables unifiedPattern
    withCommonKorePatternContext unifiedPattern
        (internalVerifyPattern
            builtinVerifier
            indexedModule
            sortVariables
            freeVariables1
            unifiedPattern
        )

{- | Verify that a 'CommonKorePattern' is well-formed Kore pattern.

    Unlike 'verifyPattern', the free variables in the pattern must be declared.

    Calls to @internalVerifyPattern@ should be wrapped in
    'withCommonKorePatternContext' or similar for errors.

 -}
internalVerifyPattern
    :: Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> CommonKorePattern
    -> Verifier (UnifiedSort, CommonKorePattern)
internalVerifyPattern
    builtinVerifier
    indexedModule
    sortVariables
    declaredVariables
  =
    applyKorePattern verifyMetaPattern verifyObjectPattern
  where
    verifyMetaPattern unverified =
        Bifunctor.bimap asUnified asKorePattern <$> verify
      where
        helpers = metaVerifyHelpers indexedModule declaredVariables
        verify =
            verifyProjectedPattern
                builtinVerifier
                indexedModule
                helpers
                sortVariables
                declaredVariables
                unverified

    verifyObjectPattern unverified =
        Bifunctor.bimap asUnified asKorePattern <$> verify
      where
        helpers = objectVerifyHelpers indexedModule declaredVariables
        verify =
            verifyProjectedPattern
                builtinVerifier
                indexedModule
                helpers
                sortVariables
                declaredVariables
                unverified

{- | Verify a projected pattern at a known level.

    Calls to @verifyProjectedPattern@ should be wrapped in 'withPatternContext'
    or similar for errors.

 -}
verifyProjectedPattern
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Pattern level Variable CommonKorePattern
    -> Verifier (Sort level, Pattern level Variable CommonKorePattern)
verifyProjectedPattern
    builtinVerifier
    indexedModule
    helpers
    declaredSortVariables
    declaredVariables
  =
    -- TODO (thomas.tuegel): If we got rid of the error monad, we could rewrite
    -- this using a recursion scheme to capture *all* the sort errors (not only
    -- the first) and return the verified pattern.
    \case
        AndPattern p -> Bifunctor.second AndPattern <$> verifyAnd' p
        ApplicationPattern p -> Bifunctor.second ApplicationPattern <$> verifyApplication' p
        BottomPattern p -> Bifunctor.second BottomPattern <$> verifyBottom' p
        CeilPattern p -> Bifunctor.second CeilPattern <$> verifyCeil' p
        DomainValuePattern p ->
            let verify =
                    verifyDomainValue
                        builtinVerifier
                        helpers
                        declaredSortVariables
            in
                Bifunctor.second DomainValuePattern <$> verify p
        EqualsPattern p -> Bifunctor.second EqualsPattern <$> verifyEquals' p
        ExistsPattern p -> Bifunctor.second ExistsPattern <$> verifyExists' p
        FloorPattern p -> Bifunctor.second FloorPattern <$> verifyFloor' p
        ForallPattern p -> Bifunctor.second ForallPattern <$> verifyForall' p
        IffPattern p -> Bifunctor.second IffPattern <$> verifyIff' p
        ImpliesPattern p -> Bifunctor.second ImpliesPattern <$> verifyImplies' p
        InPattern p -> Bifunctor.second InPattern <$> verifyIn' p
        NextPattern p -> Bifunctor.second NextPattern <$> verifyNext' p
        NotPattern p -> Bifunctor.second NotPattern <$> verifyNot' p
        OrPattern p -> Bifunctor.second OrPattern <$> verifyOr' p
        RewritesPattern p -> Bifunctor.second RewritesPattern <$> verifyRewrites' p
        StringLiteralPattern p -> Bifunctor.second StringLiteralPattern <$> verifyStringLiteral p
        CharLiteralPattern p -> Bifunctor.second CharLiteralPattern <$> verifyCharLiteral p
        TopPattern p -> Bifunctor.second TopPattern <$> verifyTop' p
        VariablePattern p -> Bifunctor.second VariablePattern <$> verifyVariable' p
  where
    -- TODO (thomas.tuegel): Put all the common parameters into a Reader and
    -- eliminate all these auxiliary definitions.
    verifyAnd' =
        verifyAnd builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyApplication' =
        verifyApplication builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyBottom' =
        verifyBottom builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyCeil' =
        verifyCeil builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyEquals' =
        verifyEquals builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyExists' =
        verifyExists builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyFloor' =
        verifyFloor builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyForall' =
        verifyForall builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyIff' =
        verifyIff builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyImplies' =
        verifyImplies builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyIn' =
        verifyIn builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyNext' =
        verifyNext builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyNot' =
        verifyNot builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyOr' =
        verifyOr builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyRewrites' =
        verifyRewrites builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyTop' =
        verifyTop builtinVerifier indexedModule helpers declaredSortVariables declaredVariables
    verifyVariable' =
        verifyVariableUsage indexedModule helpers declaredSortVariables declaredVariables

verifyTraversable
    :: (MetaOrObject level, Traversable f)
    => Lens' (f CommonKorePattern) (Sort level)  -- ^ access result sort
    -> Traversal' (f CommonKorePattern) (Sort level)  -- ^ access operand sort
    -> Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> f CommonKorePattern
    -> Verifier (Sort level, f CommonKorePattern)
verifyTraversable
    overResultSort
    traverseOperandSort
    builtinVerifier
    indexedModule
    VerifyHelpers { verifyHelpersFindSort = findSort }
    declaredSortVariables
    declaredVariables
    _unverified
  = do
    -- Verify that the result sort is well-formed.
    _unverified <- overResultSort verifySort' _unverified
    -- Verify that the operand sort is well-formed.
    _unverified <- traverseOperandSort verifySort' _unverified
    let resultSort = Lens.view overResultSort _unverified
        operandSort =
            fromMaybe resultSort
                (Lens.preview traverseOperandSort _unverified)
        operandUnifiedSort = asUnified operandSort
    -- Verify the child patterns. Return the verified pattern.
    verified <- traverse (verifyChild operandUnifiedSort) _unverified
    return (resultSort, verified)
  where
    verifySort' = verifySort findSort declaredSortVariables
    verifyChild operandUnifiedSort _child =
        withCommonKorePatternContext _child
            (do
                (childSort, _child)
                  <-
                    internalVerifyPattern
                        builtinVerifier
                        indexedModule
                        declaredSortVariables
                        declaredVariables
                        _child
                assertSameSort operandUnifiedSort childSort
                return _child
            )

verifyAnd
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> And level CommonKorePattern
    -> Verifier (Sort level, And level CommonKorePattern)
verifyAnd = verifyTraversable Lens.andSort (const pure)

verifyOr
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Or level CommonKorePattern
    -> Verifier (Sort level, Or level CommonKorePattern)
verifyOr = verifyTraversable Lens.orSort (const pure)

verifyBottom
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Bottom level CommonKorePattern
    -> Verifier (Sort level, Bottom level CommonKorePattern)
verifyBottom = verifyTraversable Lens.bottomSort (const pure)

verifyTop
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Top level CommonKorePattern
    -> Verifier (Sort level, Top level CommonKorePattern)
verifyTop = verifyTraversable Lens.topSort (const pure)

verifyCeil
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Ceil level CommonKorePattern
    -> Verifier (Sort level, Ceil level CommonKorePattern)
verifyCeil = verifyTraversable Lens.ceilResultSort Lens.ceilOperandSort

verifyFloor
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Floor level CommonKorePattern
    -> Verifier (Sort level, Floor level CommonKorePattern)
verifyFloor = verifyTraversable Lens.floorResultSort Lens.floorOperandSort

verifyEquals
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Equals level CommonKorePattern
    -> Verifier (Sort level, Equals level CommonKorePattern)
verifyEquals = verifyTraversable Lens.equalsResultSort Lens.equalsOperandSort

verifyIff
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Iff level CommonKorePattern
    -> Verifier (Sort level, Iff level CommonKorePattern)
verifyIff = verifyTraversable Lens.iffSort (const pure)

verifyImplies
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Implies level CommonKorePattern
    -> Verifier (Sort level, Implies level CommonKorePattern)
verifyImplies = verifyTraversable Lens.impliesSort (const pure)

verifyIn
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> In level CommonKorePattern
    -> Verifier (Sort level, In level CommonKorePattern)
verifyIn = verifyTraversable Lens.inResultSort Lens.inOperandSort

verifyNext
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Next level CommonKorePattern
    -> Verifier (Sort level, Next level CommonKorePattern)
verifyNext = verifyTraversable Lens.nextSort (const pure)

verifyNot
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Not level CommonKorePattern
    -> Verifier (Sort level, Not level CommonKorePattern)
verifyNot = verifyTraversable Lens.notSort Lens.notSort

verifyRewrites
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Rewrites level CommonKorePattern
    -> Verifier (Sort level, Rewrites level CommonKorePattern)
verifyRewrites = verifyTraversable Lens.rewritesSort (const pure)

verifyPatternsWithSorts
    :: Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> [UnifiedSort]
    -> [CommonKorePattern]
    -> Verifier [CommonKorePattern]
verifyPatternsWithSorts
    builtinVerifier
    indexedModule
    declaredSortVariables
    declaredVariables
    sorts
    operands
  = do
    koreFailWhen (declaredOperandCount /= actualOperandCount)
        (  "Expected "
        ++ show declaredOperandCount
        ++ " operands, but got "
        ++ show actualOperandCount
        ++ "."
        )
    zipWithM
        (\expectedSort unverified ->
             withCommonKorePatternContext unverified
                (do
                    (actualSort, verified) <-
                        internalVerifyPattern
                            builtinVerifier
                            indexedModule
                            declaredSortVariables
                            declaredVariables
                            unverified
                    assertSameSort expectedSort actualSort
                    return verified
                )
        )
        sorts
        operands
  where
    declaredOperandCount = length sorts
    actualOperandCount = length operands

verifyApplication
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Application level CommonKorePattern
    -> Verifier (Sort level, Application level CommonKorePattern)
verifyApplication
    builtinVerifier
    indexedModule
    helpers
    declaredSortVariables
    declaredVariables
    application@Application { applicationSymbolOrAlias }
  = do
    ApplicationSorts
        { applicationSortsOperands = operandSorts
        , applicationSortsResult = resultSort
        }
      <-
        verifySymbolOrAlias
            helpers
            declaredSortVariables
            applicationSymbolOrAlias
    let verifyChildren =
            verifyPatternsWithSorts
                builtinVerifier
                indexedModule
                declaredSortVariables
                declaredVariables
                (map asUnified operandSorts)
    verified <- Lens.traverseOf Lens.applicationChildren verifyChildren application
    return (resultSort, verified)

verifyBinder
    :: (MetaOrObject level, Traversable f)
    => Lens' (f CommonKorePattern) (Sort level)
    -> Lens' (f CommonKorePattern) (Variable level)
    -> Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> f CommonKorePattern
    -> Verifier (Sort level, f CommonKorePattern)
verifyBinder
    overResultSort
    overQuantifiedVariable
    builtinVerifier
    indexedModule
    helpers@VerifyHelpers { verifyHelpersFindSort }
    declaredSortVariables
    declaredVariables
    _unverified
  = do
    -- Verify that the quantified variable declaration is well-formed.
    _unverified <- overQuantifiedVariable verifyDeclaration _unverified
    -- Verify that the sort is well-formed.
    _unverified <- overResultSort verifySort' _unverified
    let
        expectedSort = Lens.view overResultSort _unverified
        variable = Lens.view overUnifiedVariable _unverified
        verify unverified =
            withCommonKorePatternContext unverified
            (do
                (actualSort, verified) <-
                    internalVerifyPattern
                        builtinVerifier
                        indexedModule
                        declaredSortVariables
                        (addDeclaredVariable variable declaredVariables)
                        unverified
                -- Check the expected sort.
                assertSameSort (asUnified expectedSort) actualSort
                return verified
            )
    -- Verify the bound pattern. Return the verified pattern.
    verified <- traverse verify _unverified
    return (expectedSort, verified)
  where
    overUnifiedVariable = overQuantifiedVariable . Lens.to asUnified
    verifyDeclaration =
        verifyVariableDeclaration indexedModule helpers declaredSortVariables
    verifySort' = verifySort verifyHelpersFindSort declaredSortVariables

verifyExists
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Exists level Variable CommonKorePattern
    -> Verifier (Sort level, Exists level Variable CommonKorePattern)
verifyExists = verifyBinder Lens.existsSort Lens.existsVariable

verifyForall
    :: MetaOrObject level
    => Builtin.DomainVerifier CommonKorePattern
    -> KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Forall level Variable CommonKorePattern
    -> Verifier (Sort level, Forall level Variable CommonKorePattern)
verifyForall = verifyBinder Lens.forallSort Lens.forallVariable

verifyVariableUsage
    :: (MetaOrObject level)
    => KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> DeclaredVariables
    -> Variable level
    -> Verifier (Sort level, Variable level)
verifyVariableUsage
    _
    helpers
    _
    _
    actual@Variable { variableName, variableSort = actualSort }
  = do
    declared@Variable { variableSort = declaredSort }
      <-
        findVariableDeclaration variableName helpers
    -- Verify that the actual sort matches the declared sort.
    koreFailWithLocationsWhen
        (actualSort /= declaredSort)
        [ actual, declared ]
        "The declared sort is different."
    return (actualSort, actual)

verifyDomainValue
    :: Builtin.DomainVerifier CommonKorePattern
    -> VerifyHelpers Object
    -> Set.Set UnifiedSortVariable
    -> DomainValue Object (BuiltinDomain CommonKorePattern)
    -> Verifier (Sort Object, DomainValue Object (BuiltinDomain CommonKorePattern))
verifyDomainValue
    builtinVerifier
    VerifyHelpers { verifyHelpersFindSort }
    declaredSortVariables
    _unverified@DomainValue { domainValueSort = actualSort }
  = do
    -- Verify that the sort is well-formed.
    _unverified <- Lens.traverseOf Lens.domainValueSort verifySort' _unverified
    -- Apply the builtin verifiers.
    verified <-
        Builtin.verifyDomainValue
            builtinVerifier
            verifyHelpersFindSort
            _unverified
    return (actualSort, verified)
  where
    verifySort' = verifySort verifyHelpersFindSort declaredSortVariables

verifyStringLiteral
    :: StringLiteral
    -> Verifier (Sort Meta, StringLiteral)
verifyStringLiteral lit = do
    return (stringMetaSort, lit)

verifyCharLiteral
    :: CharLiteral
    -> Verifier (Sort Meta, CharLiteral)
verifyCharLiteral lit = do
    return (charMetaSort, lit)

verifyVariableDeclaration
    :: MetaOrObject level
    => KoreIndexedModule atts
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> Variable level
    -> Verifier (Variable level)
verifyVariableDeclaration
    _
    VerifyHelpers { verifyHelpersFindSort }
    declaredSortVariables
    _unverified
  =
    Lens.traverseOf Lens.variableSort verifySort' _unverified
  where
    verifySort' = verifySort verifyHelpersFindSort declaredSortVariables

findVariableDeclaration
    :: (MetaOrObject level)
    => Id level
    -> VerifyHelpers level
    -> Verifier (Variable level)
findVariableDeclaration variableId helpers =
    case findVariables variableId of
        Nothing ->
            koreFailWithLocations
                [variableId]
                ("Unquantified variable: '" ++ getId variableId ++ "'.")
        Just variable -> return variable
  where
    findVariables = verifyHelpersFindDeclaredVariables helpers

verifySymbolOrAlias
    :: MetaOrObject level
    => VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> SymbolOrAlias level
    -> Verifier (ApplicationSorts level)
verifySymbolOrAlias
    helpers@VerifyHelpers
        { verifyHelpersLookupSymbolDeclaration = symbolLookup
        , verifyHelpersLookupAliasDeclaration = aliasLookup
        }
    declaredSortVariables
    symbolOrAlias
  =
    do
        maybeSentenceSymbol <- try (symbolLookup applicationId)
        maybeSentenceAlias <- try (aliasLookup applicationId)
        case (maybeSentenceSymbol, maybeSentenceAlias) of
            (Right sentenceSymbol, Left _) ->
                applicationSortsFromSymbolOrAliasSentence
                    symbolOrAlias
                    sentenceSymbol
                    helpers
                    declaredSortVariables
            (Left _, Right sentenceAlias) ->
                applicationSortsFromSymbolOrAliasSentence
                    symbolOrAlias
                    sentenceAlias
                    helpers
                    declaredSortVariables
            (Left err, Left _) -> throwError err
            (Right _, Right _) -> error
                "The (Right, Right) match should be caught by the unique names check."
  where
    applicationId = symbolOrAliasConstructor symbolOrAlias
    try :: Verifier a -> Verifier (Either (Error VerifyError) a)
    try f = catchError (Right <$> f) (return . Left)

applicationSortsFromSymbolOrAliasSentence
    :: (MetaOrObject level, SentenceSymbolOrAlias sa)
    => SymbolOrAlias level
    -> sa level pat variable
    -> VerifyHelpers level
    -> Set.Set UnifiedSortVariable
    -> Verifier (ApplicationSorts level)
applicationSortsFromSymbolOrAliasSentence
    symbolOrAlias
    sentence
    VerifyHelpers { verifyHelpersFindSort }
    declaredSortVariables
  = do
    mapM_
        (verifySort verifyHelpersFindSort declaredSortVariables)
        (symbolOrAliasParams symbolOrAlias)
    symbolOrAliasSorts (symbolOrAliasParams symbolOrAlias) sentence

{- | Produce an error message if the expected and actual sort differ.

    @assertSameSort@ is purely an assertion; it does no work beyond checking the
    equality of the provided sorts.

 -}
assertSameSort
    :: Unified Sort
    -- ^ expected sort
    -> Unified Sort
    -- ^ actual sort
    -> Verifier ()
assertSameSort (UnifiedObject expectedSort) (UnifiedObject actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= actualSort)
        [expectedSort, actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedMeta expectedSort) (UnifiedMeta actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= actualSort)
        [expectedSort, actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedMeta expectedSort) (UnifiedObject actualSort) =
    koreFailWithLocationsWhen
        (expectedSort /= patternMetaSort)
        [asUnified expectedSort, asUnified actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting meta sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got object sort"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )
assertSameSort (UnifiedObject expectedSort) (UnifiedMeta actualSort) =
    koreFailWithLocationsWhen
        (actualSort /= patternMetaSort)
        [asUnified expectedSort, asUnified actualSort]
        ((renderString . Pretty.layoutCompact)
         ("Expecting object sort"
          <+> Pretty.squotes (unparse expectedSort)
          <+> "but got meta sort"
          <+> Pretty.squotes (unparse actualSort)
          <> Pretty.dot)
        )


verifyFreeVariables
    :: CommonKorePattern -> Verifier DeclaredVariables
verifyFreeVariables unifiedPattern =
    foldM
        addFreeVariable
        emptyDeclaredVariables
        (Set.toList (freeVariables unifiedPattern))

addFreeVariable
    :: DeclaredVariables
    -> Unified Variable
    -> Verifier DeclaredVariables
addFreeVariable
    vars@DeclaredVariables { metaDeclaredVariables = metaVars }
    (UnifiedMeta v@Variable { variableName })
  = do
    _ <- checkVariable v metaVars
    return vars
        { metaDeclaredVariables = Map.insert variableName v metaVars }
addFreeVariable
    vars@DeclaredVariables { objectDeclaredVariables = objectVars }
    (UnifiedObject v@Variable { variableName })
  = do
    _ <- checkVariable v objectVars
    return vars
        { objectDeclaredVariables = Map.insert variableName v objectVars }

checkVariable
    :: Variable a
    -> Map.Map (Id a) (Variable a)
    -> Verifier VerifySuccess
checkVariable var@Variable { variableName } vars =
    case Map.lookup variableName vars of
        Nothing -> verifySuccess
        Just v ->
            koreFailWithLocations
                [v, var]
                ( (renderString . Pretty.layoutCompact)
                  ("Inconsistent free variable usage:"
                     <+> unparse v
                     <+> "and"
                     <+> unparse var
                     <> Pretty.dot
                  )
                )
