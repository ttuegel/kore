{-|
Module      : Kore.IndexedModule.Resolvers
Description : Tools for resolving IDs.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.IndexedModule.Resolvers
    ( getHeadApplicationSorts
    , getHeadAttributes
    , getSortAttributes
    , resolveSort
    , resolveAlias
    , resolveSymbol
    , resolveHook
    , resolveHooks
    , findIndexedSort
    ) where

import           Control.Monad.Except
                 ( MonadError )
import qualified Data.List as List
import qualified Data.Map as Map
import           Data.Maybe
                 ( fromMaybe )
import           Data.Proxy
                 ( Proxy (..) )
import qualified Data.Set as Set

import Kore.AST.Common
import Kore.AST.Error
       ( koreFailWithLocations )
import Kore.AST.Kore
import Kore.AST.MetaOrObject
       ( IsMetaOrObject (..), MetaOrObject, Object, isMetaOrObject )
import Kore.AST.Sentence
import Kore.ASTHelpers
       ( ApplicationSorts, symbolOrAliasSorts )
import Kore.Error
       ( Error, koreFail, printError )
import Kore.IndexedModule.IndexedModule
       ( IndexedModule (..), KoreIndexedModule, SortDescription,
       getIndexedSentence )

symbolSentencesMap
    :: MetaOrObject level
    => a level
    -> KoreIndexedModule atts
    -> Map.Map
        (Id level)
        (atts, SentenceSymbol level UnifiedPattern Variable)
symbolSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaSymbolSentences m
        IsObject -> indexedModuleObjectSymbolSentences m

aliasSentencesMap
    :: MetaOrObject level
    => a level
    -> KoreIndexedModule atts
    -> Map.Map
        (Id level)
        (atts, SentenceAlias level UnifiedPattern Variable)
aliasSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaAliasSentences m
        IsObject -> indexedModuleObjectAliasSentences m

sortSentencesMap
    :: MetaOrObject level
    => a level
    -> KoreIndexedModule atts
    -> Map.Map
        (Id level)
        (atts, SortDescription level)
sortSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaSortDescriptions m
        IsObject -> indexedModuleObjectSortDescriptions m

-- |Given a KoreIndexedModule and a head, it looks up the 'SentenceSymbol' or
-- 'SentenceAlias', and instantiates sort parameters with the arguments
-- specified by the head to obtain the corresponding 'ApplicationSorts'.
getHeadApplicationSorts
    :: MetaOrObject level
    => KoreIndexedModule atts  -- ^module representing a verified definition
    -> SymbolOrAlias level     -- ^the head we want to find sorts for
    -> ApplicationSorts level
getHeadApplicationSorts m patternHead =
    case resolveSymbol m headName of
        Right (_, sentence) ->
            case symbolOrAliasSorts headParams sentence of
                Left err     -> error (printError err)
                Right result -> result
        Left _ ->
            case resolveAlias m headName of
                Right (_, sentence) ->
                    case symbolOrAliasSorts headParams sentence of
                        Left err     -> error (printError err)
                        Right result -> result
                Left _ ->
                    error ("Head " ++ show patternHead ++ " not defined.")
  where
    headName = symbolOrAliasConstructor patternHead
    headParams = symbolOrAliasParams patternHead


-- |Given a KoreIndexedModule and a head, it looks up the 'SentenceSymbol' or
-- 'SentenceAlias', and returns its attributes.
-- FIXME: duplicated code as in getHeadApplicationSorts, i.e. use (<|>)
-- The problem is resolveSymbol and resolveAlias return different types
-- you could work around this with some rearrangement
-- but rather just change the types
getHeadAttributes
    :: MetaOrObject level
    => KoreIndexedModule atts  -- ^module representing a verified definition
    -> SymbolOrAlias level     -- ^the head we want to find sorts for
    -> atts
getHeadAttributes m patternHead =
    case resolveSymbol m headName of
        Right (atts, _) -> atts
        Left _ ->
            case resolveAlias m headName of
                Right (atts, _) -> atts
                Left _ ->
                    error ("Head " ++ show patternHead ++ " not defined.")
  where
    headName = symbolOrAliasConstructor patternHead


getSortAttributes
    :: MetaOrObject level
    => KoreIndexedModule atts
    -> Sort level
    -> atts
getSortAttributes m (SortActualSort (SortActual sortId _)) =
  case resolveSort m sortId of
    Right (atts, _) -> atts
    Left _ -> error $ "Sort " ++ show sortId ++ " not defined."
getSortAttributes _ _ = error "Can't lookup attributes for sort variables"


{-|'resolveThing' looks up an id in an 'IndexedModule', also searching in the
imported modules.
-}
resolveThing
    :: (IndexedModule sortParam pat variable atts
        -> Map.Map (Id level) (atts, thing level pat variable))
    -- ^ extracts the map into which to look up the id
    -> IndexedModule sortParam pat variable atts
    -> Id level
    -> Maybe (atts, thing level pat variable)
resolveThing
    mapExtractor
    indexedModule
    thingId
  =
    fst
        ( resolveThingInternal
            (Nothing, Set.empty) mapExtractor indexedModule thingId
        )

resolveThingInternal
    :: (Maybe (atts, thing level pat variable), Set.Set ModuleName)
    -> (IndexedModule sortParam pat variable atts
        -> Map.Map (Id level) (atts, thing level pat variable))
    -> IndexedModule sortParam pat variable atts
    -> Id level
    -> (Maybe (atts, thing level pat variable), Set.Set ModuleName)
resolveThingInternal x@(Just _, _) _ _ _ = x
resolveThingInternal x@(Nothing, searchedModules) _ indexedModule _
    | indexedModuleName indexedModule `Set.member` searchedModules = x
resolveThingInternal
    (Nothing, searchedModules)
    mapExtractor
    indexedModule
    thingId
  =
    case Map.lookup thingId things of
        Just thing -> (Just thing, undefined {- this should never evaluate -})
        Nothing ->
            foldr
                (\(_, _, m) partialResult -> resolveThingInternal
                    partialResult
                    mapExtractor
                    m
                    thingId
                )
                ( Nothing
                , Set.insert (indexedModuleName indexedModule) searchedModules
                )
                (indexedModuleImports indexedModule)
  where
    things = mapExtractor indexedModule

{-|'resolveSymbol' looks up a symbol id in an 'IndexedModule',
also searching in the imported modules.
-}
resolveSymbol
    :: (MetaOrObject level, MonadError (Error a) m)
    => KoreIndexedModule atts
    -> Id level
    -> m (atts, KoreSentenceSymbol level)
resolveSymbol m headId =
    case resolveThing (symbolSentencesMap (Proxy :: Proxy level)) m headId of
        Nothing ->
            koreFailWithLocations
                [headId]
                ("Symbol '" ++ getId headId ++  "' not defined.")
        Just result -> return result

{-|'resolveAlias' looks up a symbol id in an 'IndexedModule',
also searching in the imported modules.
-}
resolveAlias
    :: (MetaOrObject level, MonadError (Error a) m)
    => KoreIndexedModule atts
    -> Id level
    -> m (atts, KoreSentenceAlias level)
resolveAlias m headId =
    case resolveThing (aliasSentencesMap (Proxy :: Proxy level)) m headId of
        Nothing ->
            koreFailWithLocations
                [headId]
                ("Alias '" ++ getId headId ++  "' not defined.")
        Just result -> return result


{-|'resolveSort' looks up a sort id in an 'IndexedModule',
also searching in the imported modules.
-}
resolveSort
    :: (MetaOrObject level, MonadError (Error a) m)
    => KoreIndexedModule atts
    -> Id level
    -> m (atts, SortDescription level)
resolveSort m sortId =
    case resolveThing (sortSentencesMap (Proxy :: Proxy level)) m sortId of
        Nothing ->
            koreFailWithLocations
                [sortId]
                ("Sort '" ++ getId sortId ++  "' not declared.")
        Just sortDescription -> return sortDescription

resolveHook
    :: MonadError (Error a) m
    => KoreIndexedModule atts
    -> String
    -> m (Id Object)
resolveHook indexedModule builtinName =
    case resolveHooks indexedModule builtinName of
        [hookId] -> return hookId
        [] ->
            koreFail ("Builtin '" ++ builtinName ++ "' is not hooked.")
        hookIds ->
            koreFail
                ("Builtin '" ++ builtinName
                    ++ "' is hooked to multiple identifiers: "
                    ++ List.intercalate ", " (squotes . getId <$> hookIds)
                )
          where
            squotes str = "'" ++ str ++ "'"

resolveHooks
    :: KoreIndexedModule atts
    -> String
    -> [Id Object]
resolveHooks indexedModule builtinName =
    foldMap resolveHooks1 allHooks
  where
    allHooks = allHooksOf indexedModule
    allHooksOf _module =
        let _imports =
                (\(_, _, _import) -> _import) <$> indexedModuleImports _module
        in
            indexedModuleHooks _module : mconcat (allHooksOf <$> _imports)
    resolveHooks1 hooks = fromMaybe [] (Map.lookup builtinName hooks)

{- | Find a sort by name in an indexed module and its imports.

    Similar to 'resolveSort', but does not retrieve the sentence attributes.

 -}
findIndexedSort
    :: (MetaOrObject level, MonadError (Error e) m)
    => KoreIndexedModule atts
    -- ^ indexed module
    -> Id level
    -- ^ sort identifier
    -> m (SortDescription level)
findIndexedSort indexedModule sort =
    fmap getIndexedSentence (resolveSort indexedModule sort)
