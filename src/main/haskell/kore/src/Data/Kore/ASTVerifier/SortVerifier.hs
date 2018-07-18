{-|
Module      : Data.Kore.ASTVerifier.SortVerifier
Description : Tools for verifying the wellformedness of a Kore 'Sort'.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : POSIX
-}
module Data.Kore.ASTVerifier.SortVerifier (verifySort) where

import           Data.Kore.AST.Common
import           Data.Kore.AST.Sentence
import           Data.Kore.AST.Error
import           Data.Kore.AST.Kore
import           Data.Kore.AST.MetaOrObject
import           Data.Kore.ASTVerifier.Error
import           Data.Kore.Error
import           Data.Kore.IndexedModule.IndexedModule
import qualified Data.Set                              as Set

{-|'verifySort' verifies the welformedness of a Kore 'Sort'. -}
verifySort
    :: MetaOrObject level
    => (Id level -> Either (Error VerifyError) (SortDescription level))
    -- ^ Provides a sortMetaSorts description.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables visible here.
    -> Sort level
    -> Either (Error VerifyError) VerifySuccess
verifySort _ declaredSortVariables (SortVariableSort variable)
  = do
    koreFailWithLocationsWhen
        (not (unifiedVariable `Set.member` declaredSortVariables))
        [variableId]
        ("Sort variable '" ++ getId variableId ++ "' not declared.")
    verifySuccess
  where
    variableId = getSortVariable variable
    unifiedVariable = asUnified variable
verifySort findSortDescription declaredSortVariables (SortActualSort sort)
  =
    withLocationAndContext
        (sortActualName sort)
        ("sort '" ++ getId (sortActualName sort) ++ "'")
        ( do
            sortDescription <- findSortDescription (sortActualName sort)
            verifySortMatchesDeclaration
                findSortDescription
                declaredSortVariables
                sort
                sortDescription
        )

verifySortMatchesDeclaration
    :: MetaOrObject level
    => (Id level -> Either (Error VerifyError) (SortDescription level))
    -> Set.Set UnifiedSortVariable
    -> SortActual level
    -> SortDescription level
    -> Either (Error VerifyError) VerifySuccess
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
