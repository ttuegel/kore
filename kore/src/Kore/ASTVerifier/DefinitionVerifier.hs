{-|
Module      : Kore.ASTVerifier.DefinitionVerifier
Description : Tools for verifying the wellformedness of a Kore 'Definiton'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.DefinitionVerifier
    ( verifyDefinition
    , verifyAndIndexDefinition
    , verifyAndIndexDefinitionWithBase
    ) where

import Control.Monad
    ( foldM
    )
import qualified Data.Foldable as Foldable
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Text
    ( Text
    )

import Kore.ASTVerifier.AttributesVerifier hiding
    ( parseAttributes
    )
import Kore.ASTVerifier.Error
import Kore.ASTVerifier.ModuleVerifier
import Kore.ASTVerifier.Verifier
import qualified Kore.Attribute.Axiom as Attribute
import Kore.Attribute.Parser as Attribute.Parser
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin as Builtin
import Kore.Error
import Kore.IndexedModule.IndexedModule
import Kore.Syntax.Definition
import qualified Kore.Verified as Verified

{-|'verifyDefinition' verifies the welformedness of a Kore 'Definition'.

It does not fully verify the validity of object-meta combinations of patterns,
e.g.:

@
  axiom{S1,S2,R}
    \equals{Ctxt{S1,S2},R}(
      gamma{S1,S2}(
        #variableToPattern{}(#X:#Variable{}),
        #P:#Pattern{}),
      \exists{Ctxt{S1,S2}}(
        #X:#Variable{},
        gamma0{S1,S2}(
          #variableToPattern{}(#X:#Variable{}),
          #P:#Pattern{}))) []
@

-}
verifyDefinition
    :: Builtin.Verifiers
    -> ParsedDefinition
    -> Either (Error VerifyError) VerifySuccess
verifyDefinition builtinVerifiers definition = do
    _ <-
        verifyAndIndexDefinition builtinVerifiers definition
    verifySuccess

{-|'verifyAndIndexDefinition' verifies a definition and returns an indexed
collection of the definition's modules.
-}
verifyAndIndexDefinition
    :: Builtin.Verifiers
    -> ParsedDefinition
    -> Either
        (Error VerifyError)
        (Map.Map ModuleName (VerifiedModule Attribute.Symbol Attribute.Axiom))
verifyAndIndexDefinition builtinVerifiers definition = do
    (indexedModules, _defaultNames) <-
        verifyAndIndexDefinitionWithBase
            mempty
            builtinVerifiers
            definition
    return indexedModules

{-|Verifies a `ParsedDefinition` against a preverified definition, consisting of
map of indexed modules and a map of defined names.

If verification is successfull, it returns the updated maps op indexed modules
and defined names.
-}
verifyAndIndexDefinitionWithBase
    ::  (Map ModuleName VerifiedModule', Map Text AstLocation)
    ->  Builtin.Verifiers
    ->  ParsedDefinition
    ->  Either (Error VerifyError)
            (Map ModuleName VerifiedModule', Map Text AstLocation)
verifyAndIndexDefinitionWithBase
    alreadyVerified
    builtinVerifiers
    definition
  = do
    let
        (verifiedModulesCache, baseNames) =
            (implicitModules, implicitNames) <> alreadyVerified

    names <- foldM verifyUniqueNames baseNames (definitionModules definition)

    let
        implicitModule
            :: ImplicitIndexedModule Verified.Pattern Attribute.Symbol Attribute.Axiom
        implicitModule = ImplicitIndexedModule implicitIndexedModule
        parsedModules = modulesByName (definitionModules definition)
        definitionModuleNames = moduleName <$> definitionModules definition
        verifyModules = Foldable.traverse_ verifyModule definitionModuleNames

    -- Verify the contents of the definition.
    (_, index) <-
        runVerifier
            verifyModules
            verifiedModulesCache
            implicitModule
            parsedModules
            builtinVerifiers
    verifyAttributes (definitionAttributes definition)

    return (index, names)
  where
    modulesByName = Map.fromList . map (\m -> (moduleName m, m))
