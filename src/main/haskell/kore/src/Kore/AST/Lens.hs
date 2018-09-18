{-# LANGUAGE TemplateHaskell #-}

{-|
Module      : Kore.AST.Lens
Description : Lenses for pattern types
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module defines lenses into the sorts and children of matching logic pattern
types from "Kore.AST.Common". It is intended to be imported qualified, to avoid
name conflicts:

@
import qualified Kore.AST.Lens as Lens
@
-}

module Kore.AST.Lens
    ( -- * Patterns
      -- ** And
      andSort
    , andFirst
    , andSecond
      -- ** Application
    , applicationSymbolOrAlias
    , applicationChildren
      -- ** Bottom
    , bottomSort
      -- ** Ceil
    , ceilOperandSort
    , ceilResultSort
    , ceilChild
      -- ** DomainValue
    , domainValueSort
    , domainValueChild
      -- ** Equals
    , equalsOperandSort
    , equalsResultSort
    , equalsFirst
    , equalsSecond
      -- ** Exists
    , existsSort
    , existsVariable
    , existsChild
      -- ** Floor
    , floorOperandSort
    , floorResultSort
    , floorChild
      -- ** Forall
    , forallSort
    , forallVariable
    , forallChild
      -- ** Iff
    , iffSort
    , iffFirst
    , iffSecond
      -- ** Implies
    , impliesSort
    , impliesFirst
    , impliesSecond
      -- ** In
    , inOperandSort
    , inResultSort
    , inContainedChild
    , inContainingChild
      -- ** Next
    , nextSort
    , nextChild
      -- ** Not
    , notSort
    , notChild
      -- ** Or
    , orSort
    , orFirst
    , orSecond
      -- ** Rewrites
    , rewritesSort
    , rewritesFirst
    , rewritesSecond
      -- ** Top
    , topSort
      -- ** Variable
    , variableSort
    , variableName
    ) where

import qualified Kore.AST.Common as Common

import Control.Lens.TH.Simple

makeSimpleLenses ''Common.And
makeSimpleLenses ''Common.Application
makeSimpleLenses ''Common.Bottom
makeSimpleLenses ''Common.Ceil
makeSimpleLenses ''Common.DomainValue
makeSimpleLenses ''Common.Equals
makeSimpleLenses ''Common.Exists
makeSimpleLenses ''Common.Floor
makeSimpleLenses ''Common.Forall
makeSimpleLenses ''Common.Iff
makeSimpleLenses ''Common.Implies
makeSimpleLenses ''Common.In
makeSimpleLenses ''Common.Next
makeSimpleLenses ''Common.Not
makeSimpleLenses ''Common.Or
makeSimpleLenses ''Common.Rewrites
makeSimpleLenses ''Common.Top
makeSimpleLenses ''Common.Variable
