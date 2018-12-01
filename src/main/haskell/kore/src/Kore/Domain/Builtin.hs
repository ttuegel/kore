{- |
Module      : Kore.Domain.Builtin
Description : Internal domain value representation
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

This module is intended to be imported qualified:

@
    import qualified Kore.Domain.Builtin as Domain
@
 -}
{-# LANGUAGE TemplateHaskell #-}

module Kore.Domain.Builtin where

import           Control.DeepSeq
                 ( NFData (..) )
import           Data.Deriving
                 ( deriveEq1, deriveOrd1, deriveShow1 )
import qualified Data.Foldable as Foldable
import           Data.Functor.Const
                 ( Const )
import           Data.Hashable
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Sequence
                 ( Seq )
import           Data.Set
                 ( Set )
import           Data.Void
                 ( Void )
import           GHC.Generics
                 ( Generic )
import           GHC.Stack
                 ( HasCallStack )

import Kore.Annotation.Valid
import Kore.AST.Pure

type KeyPattern = ConcretePurePattern Object Builtin (Valid Object)

data Builtin child
    = BuiltinPattern !(CommonPurePattern Meta (Const Void) ())
    | BuiltinMap !(Map KeyPattern child)
    | BuiltinList !(Seq child)
    | BuiltinSet !(Set KeyPattern)
    deriving (Foldable, Functor, Generic, Traversable)

deriving instance Eq child => Eq (Builtin child)

deriving instance Ord child => Ord (Builtin child)

deriving instance Show child => Show (Builtin child)

deriveEq1 ''Builtin
deriveOrd1 ''Builtin
deriveShow1 ''Builtin

instance Hashable child => Hashable (Builtin child) where
    hashWithSalt salt =
        \case
            BuiltinPattern pat ->
                salt `hashWithSalt` (0::Int) `hashWithSalt` pat
            BuiltinMap (Map.toAscList -> map') ->
                salt `hashWithSalt` (1::Int) `hashWithSalt` map'
            BuiltinList (Foldable.toList -> list) ->
                salt `hashWithSalt` (2::Int) `hashWithSalt` list
            BuiltinSet (Foldable.toList -> set) ->
                salt `hashWithSalt` (3::Int) `hashWithSalt` set

instance NFData child => NFData (Builtin child)

{- | Throw an error for operations not implemented for internal domain values.
 -}
notImplementedInternal :: HasCallStack => a
notImplementedInternal = error "Not implemented for internal domain values"
