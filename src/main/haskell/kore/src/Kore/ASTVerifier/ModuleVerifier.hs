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
import           Data.Text
                 ( Text )

import           Kore.AST.Kore
import           Kore.AST.Sentence
import           Kore.ASTVerifier.AttributesVerifier
import           Kore.ASTVerifier.Error
import qualified Kore.ASTVerifier.SentenceVerifier as SentenceVerifier
import qualified Kore.Attribute.Sort as Attribute
import qualified Kore.Builtin as Builtin
import           Kore.Error
import           Kore.IndexedModule.IndexedModule

{-|'verifyUniqueNames' verifies that names defined in a module are unique both
within the module and outside, using the provided name set. -}
verifyUniqueNames
    :: Map.Map Text AstLocation
    -- ^ Names that are already defined.
    -> Module (UnifiedSentence param pat)
    -> Either (Error VerifyError) (Map.Map Text AstLocation)
    -- ^ On success returns the names that were previously defined together with
    -- the names defined in the given 'Module'.
verifyUniqueNames existingNames koreModule =
    withContext
        ("module '" ++ getModuleNameForError (moduleName koreModule) ++ "'")
        (SentenceVerifier.verifyUniqueNames
            (moduleSentences koreModule)
            existingNames)

{-|'verifyModule' verifies the welformedness of a Kore 'Module'. -}
verifyModule
    :: AttributesVerification declAtts axiomAtts
    -> Builtin.Verifiers
    -> KoreIndexedModule declAtts Attribute.Sort axiomAtts
    -> Either (Error VerifyError) (Module VerifiedKoreSentence)
verifyModule attributesVerification builtinVerifiers indexedModule =
    withContext
        ("module '" ++ getModuleNameForError (indexedModuleName indexedModule) ++ "'")
        (do
            verifyAttributes
                (snd (indexedModuleAttributes indexedModule))
                attributesVerification
            moduleSentences <-
                SentenceVerifier.verifySentences
                    indexedModule
                    attributesVerification
                    builtinVerifiers
                    (indexedModuleRawSentences indexedModule)
            return Module { moduleName, moduleSentences, moduleAttributes }
        )
  where
    moduleName = indexedModuleName indexedModule
    (_, moduleAttributes) = indexedModuleAttributes indexedModule
