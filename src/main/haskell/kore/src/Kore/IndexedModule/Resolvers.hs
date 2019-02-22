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
    ( HeadType(..)
    , getHeadApplicationSorts
    , getHeadAttributes
    , getHeadType
    , getSortAttributes
    , resolveSort
    , resolveAlias
    , resolveSymbol
    , resolveHook
    , resolveHooks
    , findIndexedSort
    ) where

import qualified Data.List as List
import qualified Data.Map.Strict as Map
import           Data.Proxy
                 ( Proxy (..) )
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import qualified Data.Text as Text
import           GHC.Stack
                 ( HasCallStack )

import Kore.AST.Error
       ( koreFailWithLocations )
import Kore.AST.Kore
import Kore.AST.Sentence hiding
       ( Alias (..), Symbol (..) )
import Kore.ASTHelpers
       ( ApplicationSorts (..), symbolOrAliasSorts )
import Kore.Error
import Kore.IndexedModule.IndexedModule
       ( IndexedModule (..), getIndexedSentence, indexedModulesInScope )

symbolSentencesMap
    :: MetaOrObject level
    => a level
    -> IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Map.Map
        (Id level)
        (declAtts, SentenceSymbol level patternType)
symbolSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaSymbolSentences m
        IsObject -> indexedModuleObjectSymbolSentences m

aliasSentencesMap
    :: MetaOrObject level
    => a level
    -> IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Map.Map (Id level) (declAtts, SentenceAlias level patternType)
aliasSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaAliasSentences m
        IsObject -> indexedModuleObjectAliasSentences m

sortSentencesMap
    :: MetaOrObject level
    => a level
    -> IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Map.Map (Id level) (sortAtts, SentenceSort level patternType)
sortSentencesMap a m =
    case isMetaOrObject a of
        IsMeta   -> indexedModuleMetaSortDescriptions m
        IsObject -> indexedModuleObjectSortDescriptions m

-- |Given a KoreIndexedModule and a head, it looks up the 'SentenceSymbol' or
-- 'SentenceAlias', and instantiates sort parameters with the arguments
-- specified by the head to obtain the corresponding 'ApplicationSorts'.
getHeadApplicationSorts
    :: MetaOrObject level
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -- ^ Module representing an indexed definition
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
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -- ^ module representing an indexed definition
    -> SymbolOrAlias level     -- ^the head we want to find sorts for
    -> declAtts
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

-- |The type of a 'SymbolOrAlias'.
data HeadType
    = Alias
    | Symbol

-- |Given a KoreIndexedModule and a head, retrieves the head type.
getHeadType
    :: MetaOrObject level
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -- ^ Module representing an indexed definition
    -> SymbolOrAlias level     -- ^the head we want to find sorts for
    -> HeadType
getHeadType m patternHead =
    case resolveSymbol m headName of
        Right _ -> Symbol
        Left _ ->
            case resolveAlias m headName of
                Right _ -> Alias
                Left _ ->
                    error ("Head " ++ show patternHead ++ " not defined.")
  where
    headName = symbolOrAliasConstructor patternHead

getSortAttributes
    :: (HasCallStack, MetaOrObject level)
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Sort level
    -> sortAtts
getSortAttributes m (SortActualSort (SortActual { sortActualName })) =
    case resolveSort m sortActualName of
        Right (atts, _) -> atts
        Left _ -> error $ "Sort " ++ show sortActualName ++ " not defined."
getSortAttributes _ _ = error "Can't lookup attributes for sort variables"


{-|'resolveThing' looks up an id in an 'IndexedModule', also searching in the
imported modules.
-}
resolveThing
    ::  (  IndexedModule sortParam patternType declAtts sortAtts axiomAtts
        -> Map.Map (Id level) result
        )
    -- ^ extracts the map into which to look up the id
    -> IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Id level
    -> Maybe result
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
    :: (Maybe result, Set.Set ModuleName)
    ->  (  IndexedModule sortParam patternType declAtts sortAtts axiomAtts
        -> Map.Map (Id level) result
        )
    -> IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Id level
    -> (Maybe result, Set.Set ModuleName)
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
    :: (MetaOrObject level, MonadError (Error e) m)
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Id level
    -> m (declAtts, SentenceSymbol level patternType)
resolveSymbol m headId =
    case resolveThing (symbolSentencesMap (Proxy :: Proxy level)) m headId of
        Nothing ->
            koreFailWithLocations
                [headId]
                ("Symbol '" ++ getIdForError headId ++  "' not defined.")
        Just result ->
            return result

{-|'resolveAlias' looks up a symbol id in an 'IndexedModule',
also searching in the imported modules.
-}
resolveAlias
    :: (MetaOrObject level, MonadError (Error e) m)
    => IndexedModule param pat declAtts sortAtts axiomAtts
    -> Id level
    -> m (declAtts, SentenceAlias level pat)
resolveAlias m headId =
    case resolveThing (aliasSentencesMap (Proxy :: Proxy level)) m headId of
        Nothing ->
            koreFailWithLocations
                [headId]
                ("Alias '" ++ getIdForError headId ++  "' not defined.")
        Just result ->
            return result


{-|'resolveSort' looks up a sort id in an 'IndexedModule',
also searching in the imported modules.
-}
resolveSort
    :: (MetaOrObject level, MonadError (Error e) m)
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Id level
    -> m (sortAtts, SentenceSort level patternType)
resolveSort m sortId =
    case resolveThing (sortSentencesMap (Proxy :: Proxy level)) m sortId of
        Nothing ->
            koreFailWithLocations
                [sortId]
                ("Sort '" ++ getIdForError sortId ++  "' not declared.")
        Just sortDescription ->
            return sortDescription

resolveHook
    :: IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Text
    -> Sort Object
    -> Either (Error e) (Id Object)
resolveHook indexedModule builtinName builtinSort =
    resolveHookHandler builtinName
    $ Set.filter relevant
    $ resolveHooks indexedModule builtinName
  where
    relevant name =
        involvesSort indexedModule builtinSort (SymbolOrAlias name [])

involvesSort
    :: IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Sort Object
    -> SymbolOrAlias Object
    -> Bool
involvesSort indexedModule builtinSort sym =
    elem builtinSort $
    (\s -> applicationSortsResult s : applicationSortsOperands s) $
    getHeadApplicationSorts indexedModule sym

resolveHookHandler
    :: Text
    -> Set (Id Object)
    -> Either (Error e) (Id Object)
resolveHookHandler builtinName results =
    case Set.toList results of
        [hookId] -> return hookId
        [] ->
            koreFail
                ("Builtin '" ++ Text.unpack builtinName ++ "' is not hooked.")
        hookIds ->
            koreFail
                ("Builtin '" ++ Text.unpack builtinName
                    ++ "' is hooked to multiple identifiers: "
                    ++ List.intercalate ", "
                        (squotes . getIdForError <$> hookIds)
                )
      where
        squotes str = "'" ++ str ++ "'"

resolveHooks
    :: IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -> Text
    -> Set (Id Object)
resolveHooks indexedModule builtinName =
    foldMap resolveHooks1 allHooks
  where
    allHooks = indexedModuleHooks <$> indexedModulesInScope indexedModule
    resolveHooks1 hooks =
        maybe Set.empty Set.fromList (Map.lookup builtinName hooks)

{- | Find a sort by name in an indexed module and its imports.

    Similar to 'resolveSort', but does not retrieve the sentence attributes.

 -}
findIndexedSort
    :: MetaOrObject level
    => IndexedModule sortParam patternType declAtts sortAtts axiomAtts
    -- ^ indexed module
    -> Id level
    -- ^ sort identifier
    -> Either (Error e) (SentenceSort level patternType)
findIndexedSort indexedModule sort =
    fmap getIndexedSentence (resolveSort indexedModule sort)
