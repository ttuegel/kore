{-# LANGUAGE GADTs #-}
{-|
Module      : Kore.ASTVerifier.SortVerifier
Description : Tools for verifying the wellformedness of a Kore 'Sort'.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Kore.ASTVerifier.SortVerifier
    ( verifySort
    , verifySortOf
    ) where

import           Control.Lens
                 ( Traversal' )
import qualified Control.Lens as Lens
import qualified Data.Set as Set

import Kore.AST.Common
import Kore.AST.Error
import Kore.AST.Kore
import Kore.AST.MetaOrObject
import Kore.AST.Sentence
import Kore.ASTVerifier.Verifier
import Kore.IndexedModule.IndexedModule

{- | Verify that a Kore 'Sort' is well-formed.
 -}
verifySort
    :: MetaOrObject level
    => (Id level -> Verifier (SortDescription level))
    -- ^ Provides a sortMetaSorts description.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables visible here.
    -> Sort level
    -> Verifier (Sort level)
verifySort findSort declaredSortVariables sort =
    case sort of
        SortVariableSort variable -> verifySortVariable variable
        SortActualSort actual -> verifySortActual actual
  where
    verifySortVariable variable = do
        koreFailWithLocationsWhen
            (not (unifiedVariable `Set.member` declaredSortVariables))
            [variableId]
            ("Sort variable '" ++ getId variableId ++ "' not declared.")
        return sort
      where
        variableId = getSortVariable variable
        unifiedVariable = asUnified variable

    verifySortActual actual@SortActual { sortActualName, sortActualSorts } =
        withLocationAndContext
            sortActualName
            ("sort '" ++ sortId ++ "'")
            (do
                sortDescription <- findSort sortActualName
                _ <- verifySortMatchesDeclaration
                    findSort
                    declaredSortVariables
                    actual
                    sortDescription
                koreFailWhen
                    (sortIsMeta && sortActualSorts /= [])
                    ("Malformed meta sort '" ++ sortId
                     ++ "' with non-empty Parameter sorts.")
                return sort
            )
      where
        sortIsMeta = case asUnified sort of UnifiedObject _ -> False ; UnifiedMeta _ -> True
        sortId     = getId sortActualName

{- | Verify the sorts of a traversable structure.

    Return the verified structure, possibly with updated sorts.

    See also: 'verifySort'

 -}
verifySortOf
    :: MetaOrObject level
    => Traversal' s (Sort level)
    -> (Id level -> Verifier (SortDescription level))
    -- ^ Provides a sortMetaSorts description.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables visible here.
    -> s
    -> Verifier s
verifySortOf traversal findSort declaredSortVariables =
    Lens.traverseOf traversal (verifySort findSort declaredSortVariables)

verifySortMatchesDeclaration
    :: MetaOrObject level
    => (Id level -> Verifier (SortDescription level))
    -> Set.Set UnifiedSortVariable
    -> SortActual level
    -> SortDescription level
    -> Verifier VerifySuccess
verifySortMatchesDeclaration
    findSortDescription declaredSortVariables sort sortDescription
  = do
    koreFailWhen (actualSortCount /= declaredSortCount)
        (  "Expected "
        ++ show declaredSortCount
        ++ " sort arguments, but got "
        ++ show actualSortCount
        ++ "."
        )
    mapM_
        (verifySort findSortDescription declaredSortVariables)
        (sortActualSorts sort)
    verifySuccess
  where
    actualSortCount = length (sortActualSorts sort)
    declaredSortCount = length (sentenceSortParameters sortDescription)
