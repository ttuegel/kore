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
module Kore.ASTVerifier.SortVerifier (verifySort) where

import qualified Data.Set as Set

import Kore.AST.Common
import Kore.AST.Error
import Kore.AST.Kore
import Kore.AST.MetaOrObject
import Kore.AST.Sentence
import Kore.ASTVerifier.Verifier
import Kore.IndexedModule.IndexedModule

{-|'verifySort' verifies the welformedness of a Kore 'Sort'. -}
verifySort
    :: MetaOrObject level
    => (Id level -> Verifier (SortDescription level))
    -- ^ Provides a sortMetaSorts description.
    -> Set.Set UnifiedSortVariable
    -- ^ Sort variables visible here.
    -> Sort level
    -> Verifier VerifySuccess
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
verifySort findSortDescription declaredSortVariables (SortActualSort sort) =
    do
        _ <- withLocationAndContext
            sortName
            ("sort '" ++ getId (sortActualName sort) ++ "'")
            (do
                sortDescription <- findSortDescription sortName
                _ <- verifySortMatchesDeclaration
                    findSortDescription
                    declaredSortVariables
                    sort
                    sortDescription
                koreFailWhen
                    (sortIsMeta && sortActualSorts sort /= [])
                    ("Malformed meta sort '" ++ sortId
                     ++ "' with non-empty Parameter sorts.")
            )
        verifySuccess
  where
    sortIsMeta = case asUnified sort of UnifiedObject _ -> False ; UnifiedMeta _ -> True
    sortName   = sortActualName sort
    sortId     = getId sortName

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
