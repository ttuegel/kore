{-# LANGUAGE TemplateHaskell #-}

module Kore.IndexedModule.Indexer where

import qualified Control.Lens as Lens
import           Control.Lens.TH
                 ( makeLenses )
import           Control.Monad.Except
                 ( MonadError )
import           Control.Monad.State
                 ( MonadState, StateT, runStateT )
import           Data.Default
                 ( Default )
import           Data.Map
                 ( Map )
import qualified Data.Map as Map

import           Kore.AST.Common
                 ( Variable )
import           Kore.AST.Kore
                 ( UnifiedPattern, UnifiedSortVariable )
import           Kore.AST.Sentence
                 ( KoreModule, Module (..), ModuleName (..) )
import           Kore.Attribute.Parser
                 ( ParseAttributes, parseAttributes )
import           Kore.Error
                 ( Error )
import qualified Kore.Error
import           Kore.IndexedModule.IndexedModule hiding ( indexModuleIfNeeded )

data IndexerState sorts pat var attrs =
    IndexerState
        { _done :: Map ModuleName (IndexedModule sorts pat var attrs)
        , _working :: [IndexedModule sorts pat var attrs]
        }

makeLenses ''IndexerState

type KoreIndexerState = IndexerState UnifiedSortVariable UnifiedPattern Variable

newtype Indexer sorts pat var attrs a =
    Indexer
    { getIndexer
        :: StateT (IndexerState sorts pat var attrs)
            (Either (Error IndexModuleError)) a
    }
  deriving (Applicative, Functor, Monad)

deriving instance
    MonadState (IndexerState sorts pat var attrs) (Indexer sorts pat var attrs)
deriving instance MonadError (Error IndexModuleError) (Indexer sorts pat var attrs)

type KoreIndexer = Indexer UnifiedSortVariable UnifiedPattern Variable

runIndexer
    :: Default attrs
    => Indexer sorts pat var attrs a
    -> ImplicitIndexedModule sorts pat var attrs
    -> Either (Error IndexModuleError) (a, IndexerState sorts pat var attrs)
runIndexer Indexer { getIndexer } (ImplicitIndexedModule lastIndexedModule) =
    runStateT getIndexer initialState
  where
    moduleName = indexedModuleName lastIndexedModule
    initialState =
        IndexerState
            { _done = Map.singleton moduleName lastIndexedModule
            , _working = []
            }

{-|'indexImplicitModule' indexes a module containing implicit definitions, adds
it to the map of defined modules and returns the new map together with the
indexed module.

It imports the module provided as an argument, which means that it contains all
the symbols defined directly or indirectly in it. This makes it suitable for
creating a chain of implicit modules, each including its predecessor, with
the top one containing the symbols defined in all of them.
-}
indexImplicitModule
    :: ParseAttributes attrs
    => KoreImplicitIndexedModule attrs
    -> KoreModule
    -> KoreIndexer attrs (KoreImplicitIndexedModule attrs)
indexImplicitModule implicitModule koreModule = do
    ImplicitIndexedModule <$> indexModuleIfNeeded implicitModule koreModule

indexModuleIfNeeded
    :: ParseAttributes attrs
    => KoreImplicitIndexedModule attrs
    -> KoreModule
    -> KoreIndexer attrs (KoreIndexedModule attrs)
indexModuleIfNeeded
    implicitModule
    koreModule@Module { moduleName }
  = do
        lookup <- Lens.use (done . Lens.at moduleName)
        case lookup of
            Nothing -> indexModule implicitModule koreModule
            Just current -> return current

indexedModuleNames :: Lens.Getter [IndexedModule sorts pat var attrs] [ModuleName]
indexedModuleNames = Lens.to (fmap indexedModuleName)

beginModule :: Default attrs => KoreImplicitIndexedModule attrs -> ModuleName -> KoreIndexer attrs ()
beginModule implicitModule moduleName = do
    workingModuleNames <- Lens.use (working . indexedModuleNames)
    Kore.Error.koreFailWhen
        (elem moduleName workingModuleNames)
        "Circular module import dependency."
    let next = indexedModuleWithDefaultImports moduleName implicitModule
    working Lens.%= (:) next

indexModule
    :: ParseAttributes attrs
    => KoreImplicitIndexedModule attrs
    -> KoreModule
    -> KoreIndexer attrs (KoreIndexedModule attrs)
indexModule
    implicitModule
    koreModule@Module { moduleName }
  =
    Kore.Error.withContext
        ("module '" ++ getModuleName moduleName ++ "'")
        (do
            beginModule implicitModule moduleName
            return _
        )
