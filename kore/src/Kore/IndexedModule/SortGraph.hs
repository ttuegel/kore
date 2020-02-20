{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.IndexedModule.SortGraph
    ( SortGraph
    , fromSubsorts
    , fromIndexedModule
    , subsortsOf
    , isSubsortOf
    --
    , Subsort (..)
    ) where

import Prelude.Kore

import qualified Data.Graph.Inductive.Graph as Graph
import Data.Graph.Inductive.PatriciaTree
    ( Gr
    )
import qualified Data.Graph.Inductive.Query.DFS as Graph.Query.DFS
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Debug
import Kore.Attribute.Subsort
    ( Subsort (..)
    )
import Kore.IndexedModule.IndexedModule
    ( IndexedModule
    , indexedModuleSubsorts
    )
import Kore.Sort

{- | 'SortGraph' represents the partial order on sorts.
 -}
newtype SortGraph = SortGraph { unSortGraph :: Map Sort (Set Sort) }
    deriving (GHC.Generic, Typeable)

instance SOP.Generic SortGraph

instance SOP.HasDatatypeInfo SortGraph

instance Debug SortGraph

instance Diff SortGraph

{- | Build a 'SortGraph' from a list of 'Subsort' relations.
 -}
fromSubsorts
    :: [Subsort]  -- ^ direct subsort relations
    -> SortGraph
fromSubsorts relations =
    let supersorts = supersort <$> relations
        subsorts = subsort <$> relations
        sorts = supersorts <> subsorts
        nodes = Vector.fromList sorts

        lnodes :: [Graph.LNode Sort]
        lnodes = Vector.toList $ Vector.indexed nodes

        nodeMap :: Map Sort Int
        nodeMap = Map.fromList (map swap lnodes)
          where
            swap (x, y) = (y, x)

        edges :: [Graph.LEdge ()]
        edges =
            [ (nodeMap Map.! supersort, nodeMap Map.! subsort, ())
            | Subsort { supersort, subsort } <- relations
            ]

        sortGraph :: Gr Sort ()
        sortGraph = Graph.mkGraph lnodes edges

        reachable :: Sort -> Set Sort
        reachable sort =
            let node = nodeMap Map.! sort
                children = Graph.Query.DFS.reachable node sortGraph
                childrenSorts = map (nodes Vector.!) children
            in Set.fromList childrenSorts

    in SortGraph $ Map.fromSet reachable (Set.fromList supersorts)

fromIndexedModule
    :: IndexedModule patternType symbolAttrs axiomAttrs
    -> SortGraph
fromIndexedModule = fromSubsorts . indexedModuleSubsorts
{-# INLINE fromIndexedModule #-}

{- | Find the transitive subsorts of the given 'Sort'.
 -}
subsortsOf :: SortGraph -> Sort -> Set Sort
subsortsOf (SortGraph subsortMap) sort =
    Map.lookup sort subsortMap
    -- If the Sort is not present in the map, it was not the supersort in any of
    -- the relations used to construct the SortGraph.
    & fromMaybe Set.empty

{- | Is the first 'Sort' a subsort (transitively) of the second?
 -}
isSubsortOf
    :: SortGraph
    -> Sort  -- ^ (conjectured) subsort
    -> Sort  -- ^ (conjectured) supersort
    -> Bool
isSubsortOf sortGraph psub psuper =
    Set.member psub (subsortsOf sortGraph psuper)
