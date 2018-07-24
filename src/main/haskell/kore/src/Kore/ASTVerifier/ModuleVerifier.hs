{-|
Module      : Kore.ASTVerifier.ModuleVerifier
Description : Tools for verifying the wellformedness of a Kore 'Module'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.ModuleVerifier
    ( verifyModule
    , verifyUniqueNames
    ) where

import qualified Data.Map as Map

import           Kore.AST.Common
import           Kore.AST.Sentence
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import qualified Kore.ASTVerifier.SentenceVerifier as SentenceVerifier
import           Kore.Error
import           Kore.IndexedModule.IndexedModule

{-|'verifyUniqueNames' verifies that names defined in a module are unique both
within the module and outside, using the provided name set. -}
verifyUniqueNames
    :: Map.Map String AstLocation
    -- ^ Names that are already defined.
    -> KoreModule
    -> Either (Error VerifyError) (Map.Map String AstLocation)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames existingNames koreModule =
    withContext
        ("module '" ++ getModuleName (moduleName koreModule) ++ "'")
        (SentenceVerifier.verifyUniqueNames
            (moduleSentences koreModule)
            existingNames)

{-|'verifyModule' verifies the welformedness of a Kore 'Module'. -}
verifyModule
    :: AttributesVerification
    -> KoreIndexedModule
    -> Either (Error VerifyError) VerifySuccess
verifyModule attributesVerification indexedModule =
    withContext
        ("module '" ++ getModuleName (indexedModuleName indexedModule) ++ "'")
        (do
            verifyAttributes
                (indexedModuleAttributes indexedModule)
                attributesVerification
            SentenceVerifier.verifySentences
                indexedModule
                attributesVerification
                (indexedModuleRawSentences indexedModule)
        )
