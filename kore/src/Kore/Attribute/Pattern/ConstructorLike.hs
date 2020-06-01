{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.ConstructorLike
    ( ConstructorLike (..)
    , ConstructorLikeHead (..)
    , HasConstructorLike (..)
    , assertConstructorLike
    ) where

import Prelude.Kore

import Control.DeepSeq
import qualified Data.Map.Strict as Map
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Domain.Builtin
import Kore.Internal.Alias
    ( Alias
    )
import Kore.Internal.Inj
    ( Inj (..)
    )
import Kore.Internal.InternalBytes
    ( InternalBytes
    )
import Kore.Internal.Symbol
    ( Symbol
    )
import qualified Kore.Internal.Symbol as Symbol
import Kore.Syntax
import Kore.Syntax.Application
    ( Application (..)
    )

{- | A pattern is 'ConstructorLike' if logical equality is syntactic equality.

In other words, a pattern is constructor-like if it is equal (in the logical
'Equals' sense) to another constructor-like pattern if and only if it is
syntactically equal (in the 'Eq' sense).

Examples of patterns that are constructor-like:

* 'BuiltinBool', 'BuiltinInt', 'BuiltinString', and 'InternalBytes'
* 'StringLiteral'
* constructors with constructor-like arguments
* 'DomainValue' in a non-hooked sort
* 'Inj' in its normal form (if its argument is not also 'Inj')

Examples of patterns that are not constructor-like:

* variables
* function symbols
* logical connectives

-}
newtype ConstructorLike =
    ConstructorLike
        { getConstructorLike :: Maybe ConstructorLikeHead
        }
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic ConstructorLike

instance SOP.HasDatatypeInfo ConstructorLike

instance Debug ConstructorLike

instance Diff ConstructorLike where
    diffPrec = diffPrecIgnore

instance NFData ConstructorLike

instance Hashable ConstructorLike

instance Synthetic ConstructorLike (And sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Bottom sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Application Symbol) where
    synthetic application
        -- The constructor application is constructor-like if all
        -- its children are constructor-like.
        | Symbol.isConstructor symbol
        , childrenAreConstructorLike =
            ConstructorLike . Just $ ConstructorLikeHead

        -- Checks that the children of a sort injection are
        -- not sort injections, i.e. that the triangle axiom
        -- for sort injections has been fully applied.
        | Symbol.isSortInjection symbol
        , childrenAreConstructorLike
        , childrenAreNotSortInjections =
            ConstructorLike . Just $ SortInjectionHead

        | otherwise =
            ConstructorLike Nothing
      where
        symbol = applicationSymbolOrAlias application
        children = applicationChildren application
        childrenAreConstructorLike =
            ConstructorLike Nothing `notElem` children
        childrenAreNotSortInjections =
            (ConstructorLike . Just $ SortInjectionHead) `notElem` children

instance Synthetic ConstructorLike (Application (Alias patternType)) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Ceil sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

{- |
A domain value is not technically a constructor, but it is constructor-like for
builtin domains, at least from the perspective of normalization (normalized
means constructor-like here).
 -}
instance Synthetic ConstructorLike (DomainValue sort) where
    synthetic domainValue
        | isJust . getConstructorLike $ child =
            ConstructorLike . Just $ ConstructorLikeHead
        | otherwise =
            ConstructorLike Nothing
      where
        child = domainValueChild domainValue

instance Synthetic ConstructorLike (Equals sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Exists sort variable) where
    synthetic = const (ConstructorLike Nothing)

instance Synthetic ConstructorLike (Floor sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Forall sort variable) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Iff sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Implies sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (In sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Mu sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Next sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Not sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Nu sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Or sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Rewrites sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

{- |
A builtin value is not technically a constructor, but it is constructor-like for
builtin domains, at least from the perspective of normalization (normalized
means constructor-like here).
 -}
instance HasConstructorLike key => Synthetic ConstructorLike (Builtin key)
  where
    synthetic =
        \case
            BuiltinInt _    -> ConstructorLike . Just $ ConstructorLikeHead
            BuiltinBool _   -> ConstructorLike . Just $ ConstructorLikeHead
            BuiltinString _ -> ConstructorLike . Just $ ConstructorLikeHead
            (BuiltinMap InternalAc
                    {builtinAcChild = NormalizedMap builtinMapChild}
                ) -> normalizedAcConstructorLike builtinMapChild
            (BuiltinSet InternalAc
                    {builtinAcChild = NormalizedSet builtinSetChild}
                ) -> normalizedAcConstructorLike builtinSetChild
            _               -> ConstructorLike Nothing
    {-# INLINE synthetic #-}

normalizedAcConstructorLike
    ::  ( HasConstructorLike key
        , HasConstructorLike (Value collection ConstructorLike)
        )
    => NormalizedAc collection key ConstructorLike -> ConstructorLike
normalizedAcConstructorLike ac@(NormalizedAc _ _ _) =
    case ac of
        NormalizedAc
            { elementsWithVariables = []
            , concreteElements
            , opaque = []
            }
              | all pairIsConstructorLike concreteElementsList
                -> ConstructorLike . Just $ ConstructorLikeHead
              where
                concreteElementsList = Map.toList concreteElements
                pairIsConstructorLike (key, value) =
                    assertConstructorLike "" key $ isConstructorLike value
        _ -> ConstructorLike Nothing

instance Synthetic ConstructorLike Inhabitant where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const (SomeVariable variable)) where
    synthetic = const (ConstructorLike Nothing)

instance Synthetic ConstructorLike (Const StringLiteral) where
    synthetic = const (ConstructorLike . Just $ ConstructorLikeHead)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Const InternalBytes) where
    synthetic = const (ConstructorLike . Just $ ConstructorLikeHead)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike (Top sort) where
    synthetic = const (ConstructorLike Nothing)
    {-# INLINE synthetic #-}

instance Synthetic ConstructorLike Inj where
    synthetic Inj { injChild } = ConstructorLike $ do
        childHead <- getConstructorLike injChild
        case childHead of
            SortInjectionHead -> Nothing
            _                 -> pure SortInjectionHead
    {-# INLINE synthetic #-}

data ConstructorLikeHead = ConstructorLikeHead
                         | SortInjectionHead
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic ConstructorLikeHead

instance SOP.HasDatatypeInfo ConstructorLikeHead

instance Debug ConstructorLikeHead

instance Diff ConstructorLikeHead where
    diffPrec = diffPrecIgnore

instance NFData ConstructorLikeHead

instance Hashable ConstructorLikeHead

class HasConstructorLike a where
    extractConstructorLike :: a -> ConstructorLike

    isConstructorLike :: a -> Bool
    isConstructorLike a = case extractConstructorLike a of
        (ConstructorLike constructorLike) -> isJust constructorLike

instance HasConstructorLike ConstructorLike where
    extractConstructorLike = id

instance HasConstructorLike (Value NormalizedMap ConstructorLike) where
    extractConstructorLike (MapValue result) = result

instance HasConstructorLike (Value NormalizedSet ConstructorLike) where
    extractConstructorLike SetValue =
        ConstructorLike . Just $ ConstructorLikeHead

assertConstructorLike :: HasConstructorLike a => String -> a -> b -> b
assertConstructorLike message a =
    if not (isConstructorLike a)
    then error ("Expecting constructor-like object. " ++ message)
    else id
