{-|
Module      : Kore.Variables.Free
Description : Specifies the 'TermWithVariablesClass' which is meant to define
              a term with variables and exports 'freeVariables'
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.Variables.Free
    ( freeVariables
    , freePureVariables
    , allVariables
    , pureAllVariables
    ) where

import qualified Control.Comonad.Trans.Cofree as Cofree
import qualified Control.Monad as Monad
import           Control.Monad.RWS.Strict
                 ( RWS )
import qualified Control.Monad.RWS.Strict as Monad.RWS
import           Control.Monad.State.Strict
                 ( State )
import qualified Control.Monad.State.Strict as State
import qualified Data.Foldable as Foldable
import           Data.Functor.Foldable
                 ( Recursive )
import qualified Data.Functor.Foldable as Recursive
import           Data.Set
                 ( Set )
import qualified Data.Set as Set

import Kore.AST.Pure

-- | The free variables of a pure pattern.
freePureVariables
    :: (Foldable dom, Functor dom, Ord (var lvl))
    => PurePattern lvl dom var ann
    -> Set (var lvl)
freePureVariables root =
    let (free, ()) =
            Monad.RWS.execRWS
                (freePureVariables1 root)
                Set.empty  -- initial set of bound variables
                Set.empty  -- initial set of free variables
    in
        free
  where
    unlessM m go = m >>= \b -> Monad.unless b go
    isBound v = Monad.RWS.asks (Set.member v)
    recordFree v = Monad.RWS.modify' (Set.insert v)

    freePureVariables1 recursive =
        case Cofree.tailF (Recursive.project recursive) of
            VariablePattern v -> unlessM (isBound v) (recordFree v)
            ExistsPattern Exists { existsVariable, existsChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert existsVariable)
                    -- descend into the bound pattern
                    (freePureVariables1 existsChild)
            ForallPattern Forall { forallVariable, forallChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert forallVariable)
                    -- descend into the bound pattern
                    (freePureVariables1 forallChild)
            p -> mapM_ freePureVariables1 p

-- | The free variables of a pattern.
freeVariables
    ::  forall t a pat dom var.
        ( UnifiedPatternInterface pat
        , Functor (pat dom var)
        , Foldable dom
        , OrdMetaOrObject var
        , Recursive t
        , Base t ~ CofreeF (pat dom var) a
        )
    => t -> Set.Set (Unified var)
freeVariables root =
    let (free, ()) =
            Monad.RWS.execRWS
                (freeVariables1 root)
                Set.empty  -- initial set of bound variables
                Set.empty  -- initial set of free variables
    in
        free
  where
    unlessM m go = m >>= \b -> Monad.unless b go
    isBound v = Monad.RWS.asks (Set.member $ asUnified v)
    recordFree v = Monad.RWS.modify' (Set.insert $ asUnified v)

    freeVariables1 recursive =
        unifiedPatternApply freeVariables2
        $ Cofree.tailF $ Recursive.project recursive

    freeVariables2
        :: MetaOrObject lvl
        => Pattern lvl dom var t
        -> RWS (Set.Set (Unified var)) () (Set.Set (Unified var)) ()
    freeVariables2 =
        \case
            VariablePattern v -> unlessM (isBound v) (recordFree v)
            ExistsPattern Exists { existsVariable, existsChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert $ asUnified existsVariable)
                    -- descend into the bound pattern
                    (freeVariables1 existsChild)
            ForallPattern Forall { forallVariable, forallChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert $ asUnified forallVariable)
                    -- descend into the bound pattern
                    (freeVariables1 forallChild)
            p -> mapM_ freeVariables1 p


-- | The free variables of a pattern.
allVariables
    ::  forall t a pat dom var.
        ( UnifiedPatternInterface pat
        , Functor (pat dom var)
        , Foldable dom
        , OrdMetaOrObject var
        , Recursive t
        , Base t ~ CofreeF (pat dom var) a
        )
    => t -> Set.Set (Unified var)
allVariables root =
    State.execState
        (allVariables1 root)
        Set.empty  -- initial set of all variables
  where
    record v = State.modify' (Set.insert $ asUnified v)

    allVariables1 recursive =
        unifiedPatternApply allVariables2
        $ Cofree.tailF $ Recursive.project recursive

    allVariables2
        :: MetaOrObject lvl
        => Pattern lvl dom var t
        -> State (Set.Set (Unified var)) ()
    allVariables2 =
        \case
            VariablePattern variable -> record variable
            ExistsPattern Exists { existsVariable, existsChild } -> do
                record existsVariable
                allVariables1 existsChild
            ForallPattern Forall { forallVariable, forallChild } -> do
                record forallVariable
                allVariables1 forallChild
            p -> mapM_ allVariables1 p

pureMergeVariables
    :: (Foldable dom, Ord (var lvl))
    => Base (PurePattern lvl dom var ann) (Set.Set (var lvl))
    -> Set.Set (var lvl)
pureMergeVariables base =
    case Cofree.tailF base of
        VariablePattern v -> Set.singleton v
        ExistsPattern Exists { existsVariable, existsChild } ->
            Set.insert existsVariable existsChild
        ForallPattern Forall { forallVariable, forallChild } ->
            Set.insert forallVariable forallChild
        p -> Foldable.foldl' Set.union Set.empty p

{-| 'pureAllVariables' extracts all variables of a given level in a pattern as a
set, regardless of whether they are quantified or not.
-}
pureAllVariables
    :: (Foldable dom, Functor dom, Ord (var lvl))
    => PurePattern lvl dom var ann -> Set.Set (var lvl)
pureAllVariables = Recursive.fold pureMergeVariables
