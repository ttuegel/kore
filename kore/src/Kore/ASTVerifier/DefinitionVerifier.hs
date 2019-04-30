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
    ( defaultAttributesVerification
    , defaultNullAttributesVerification
    , verifyDefinition
    , verifyAndIndexDefinition
    , verifyImplicitParsedDefinition
    , verifyNormalParsedDefinition
    , AttributesVerification (..)
    ) where

import           Control.Monad
                 ( foldM )
import qualified Data.Map as Map
import           Data.Proxy
                 ( Proxy (..) )

import           Kore.AST.Pure as AST.Pure
import           Kore.AST.Sentence
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import           Kore.ASTVerifier.ModuleVerifier
import qualified Kore.Attribute.Null as Attribute
import           Kore.Attribute.Parser
                 ( ParseAttributes (..) )
import qualified Kore.Builtin as Builtin
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.Parser.Sentence
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
    :: (ParseAttributes declAtts, ParseAttributes axiomAtts)
    => AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> ParsedDefinition
    -> Either (Error VerifyError) VerifySuccess
verifyDefinition attributesVerification builtinVerifiers definition = do
    _ <- verifyAndIndexDefinition attributesVerification builtinVerifiers definition
    verifySuccess

{- | Verify a 'ParsedDefinition' and generate a 'VerifiedModule'.

 -}
verifyAndIndexDefinition
    :: forall declAtts axiomAtts
    .  (ParseAttributes declAtts, ParseAttributes axiomAtts)
    => AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> ParsedDefinition
    -> Either (Error VerifyError)
        (Map.Map ModuleName (VerifiedModule declAtts axiomAtts))
verifyAndIndexDefinition
    attributesVerification
    builtinVerifiers
    definition
  = do
    _ <- foldM verifyUniqueNames implicitNames (definitionModules definition)

    let
        unverifiedDefaultModule :: KoreImplicitIndexedModule declAtts axiomAtts
        unverifiedDefaultModule = ImplicitIndexedModule implicitIndexedModule
        indexModules
            :: [ParsedModule]
            -> Either
                (Error VerifyError)
                (Map.Map ModuleName (KoreIndexedModule declAtts axiomAtts))
        indexModules modules =
            castError $ foldM
                (indexModuleIfNeeded
                    (Just unverifiedDefaultModule)
                    unverifiedModulesByName
                )
                implicitModules
                modules
          where
            unverifiedModulesByName = modulesByName modules

        verifyModule' = verifyModule attributesVerification builtinVerifiers
        verifyModules = traverse verifyModule'

    -- Index the unverified modules.
    indexedModules <- indexModules (definitionModules definition)

    -- Verify the contents of the definition.
    verifiedModules <- verifyModules (Map.elems indexedModules)
    verifyAttributes
        (definitionAttributes definition)
        attributesVerification

    let
        verifiedDefaultModule
            :: ImplicitIndexedModule
                SortVariable
                Verified.Pattern
                declAtts
                axiomAtts
        verifiedDefaultModule = ImplicitIndexedModule implicitIndexedModule
        indexVerifiedModules
            :: [Module Verified.Sentence]
            -> Either
                (Error VerifyError)
                (Map.Map ModuleName (VerifiedModule declAtts axiomAtts))
        indexVerifiedModules modules =
            castError $ foldM
                (indexModuleIfNeeded
                    (Just verifiedDefaultModule)
                    verifiedModulesByName
                )
                implicitModules
                modules
          where
            verifiedModulesByName = modulesByName modules

    -- Re-index the (now verified) modules.
    reindexedModules <- indexVerifiedModules verifiedModules
    return reindexedModules
  where
    modulesByName = Map.fromList . map (\m -> (moduleName m, m))

defaultAttributesVerification
    :: (ParseAttributes declAtts, ParseAttributes axiomAtts)
    => Proxy declAtts
    -> Proxy axiomAtts
    -> AttributesVerification declAtts axiomAtts
defaultAttributesVerification = VerifyAttributes

-- |default option for verifying attributes without parsing them
defaultNullAttributesVerification
    :: AttributesVerification Attribute.Null Attribute.Null
defaultNullAttributesVerification =
   defaultAttributesVerification proxy proxy
  where
    proxy :: Proxy Attribute.Null
    proxy = Proxy

{-|'verifyNormalParsedDefinition' is meant to be used only in the
"Kore.Implicit" package. It verifies the correctness of a definition
containing only the 'kore' default module.
-}
verifyNormalParsedDefinition
    ::  ( ParseAttributes declAtts
        , Show declAtts
        , ParseAttributes axiomAtts
        , Show axiomAtts
        )
    => AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> ParsedDefinition
    -> Either (Error VerifyError) (VerifiedModule declAtts axiomAtts)
verifyNormalParsedDefinition
    attributesVerification
    builtinVerifiers
    definition
  = do
    -- VerifyDefinition already checks the Kore module, so we skip it.
    modules <-
        verifyAndIndexDefinition
            attributesVerification
            builtinVerifiers
            definition
    name <- extractSingleModuleNameFromDefinition definition
    findModule name modules

{-|'verifyImplicitParsedDefinition' is meant to be used only in the
"Kore.Implicit" package. It verifies the correctness of a definition
containing only the 'kore' default module.
-}
verifyImplicitParsedDefinition
    :: (ParseAttributes declAtts, ParseAttributes axiomAtts)
    => AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> ParsedDefinition
    -> Either (Error VerifyError) (VerifiedModule declAtts axiomAtts)
verifyImplicitParsedDefinition
    attributesVerification
    builtinVerifiers
    definition
  = do
    modules <-
        verifyAndIndexDefinition
            attributesVerification
            builtinVerifiers
            definition { definitionModules = [] }
    name <- extractSingleModuleNameFromDefinition definition
    findModule name modules

extractSingleModuleNameFromDefinition
    :: ParsedDefinition
    -> Either (Error VerifyError) ModuleName
extractSingleModuleNameFromDefinition definition =
    case definitionModules definition of
        [] ->
            koreFail
                (  "The kore implicit definition should have exactly"
                ++ " one module, but found none."
                )
        [a] -> return (moduleName a)
        _ ->
            koreFail
                (  "The kore implicit definition should have exactly"
                ++ " one module, but found multiple ones."
                )

findModule
    :: ModuleName
    -> Map.Map ModuleName (IndexedModule param pat declAtts axiomAtts)
    -> Either (Error VerifyError) (IndexedModule param pat declAtts axiomAtts)
findModule name modules =
    case Map.lookup name modules of
        Just a -> return a
        Nothing ->
            koreFail
                (  "Internal error: the kore module ("
                ++ getModuleNameForError name
                ++ ") was not indexed ("
                ++ show (Map.keys modules)
                ++ ")."
                )
