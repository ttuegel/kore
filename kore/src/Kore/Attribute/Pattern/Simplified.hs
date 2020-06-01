{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

 -}

module Kore.Attribute.Pattern.Simplified
    ( Simplified (..)
    , isSimplified
    , isFullySimplified
    , simplifiedTo
    , fullySimplified
    , simplifiedConditionally
    , simplifiableConditionally
    , unparseTag
    ) where

import Prelude.Kore

import Control.DeepSeq
import Data.Foldable as Foldable
import Data.Text
    ( Text
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Synthetic
import Kore.Debug
import Kore.Domain.Builtin
import Kore.Internal.Inj
    ( Inj
    )
import qualified Kore.Internal.Inj as Inj
import Kore.Internal.InternalBytes
    ( InternalBytes
    )
import qualified Kore.Internal.SideCondition.SideCondition as SideCondition
    ( Representation
    )
import Kore.Syntax
    ( And
    , Application
    , Bottom
    , Ceil
    , Const
    , DomainValue
    , Equals
    , Exists
    , Floor
    , Forall
    , Iff
    , Implies
    , In
    , Inhabitant
    , Mu
    , Next
    , Not
    , Nu
    , Or
    , Rewrites
    , StringLiteral
    , Top
    )
import Kore.Syntax.Variable

{- | How well simplified is a pattern.
-}
data Type
    = Fully
    -- ^ The entire pattern is simplified
    | Partly
    -- ^ The pattern's subterms are either fully simplified or partly
    -- simplified. Normally all the leaves in a partly simplified
    -- subterm tree are fully simplified.
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic Type

instance SOP.HasDatatypeInfo Type

instance Debug Type

instance Diff Type where
    diffPrec = diffPrecIgnore

instance NFData Type

instance Hashable Type

instance Semigroup Type
  where
    Partly <> _ = Partly
    _ <> Partly = Partly

    Fully <> Fully = Fully

instance Monoid Type where
    mempty = Fully

{- | Under which condition is a pattern simplified.
-}
data Condition
    = Any
    -- ^ The term and all its subterms are simplified the same regardless
    -- of the side condition.
    | Condition !SideCondition.Representation
    -- ^ The term is in its current simplified state only when using the
    -- given side condition. When the side condition changes, e.g. by
    -- adding extra conditions, then we may be able to further simplify the
    -- term.
    | Unknown
    -- ^ Parts of the term are simplified under different side conditions.
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic Condition

instance SOP.HasDatatypeInfo Condition

instance Debug Condition

instance Diff Condition where
    diffPrec = diffPrecIgnore

instance NFData Condition

instance Hashable Condition

instance Semigroup Condition
  where
    Unknown <> _ = Unknown
    _ <> Unknown = Unknown

    Any <> c = c
    c <> Any = c

    c@(Condition c1) <> Condition c2 =
        if c1 == c2
            then c
            else Unknown

instance Monoid Condition where
    mempty = Any

data SimplifiedData =
    SimplifiedData
        { sType :: !Type
        , condition :: !Condition
        }
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic SimplifiedData

instance SOP.HasDatatypeInfo SimplifiedData

instance Debug SimplifiedData

instance Diff SimplifiedData where
    diffPrec = diffPrecIgnore

instance NFData SimplifiedData

instance Hashable SimplifiedData

{- | A pattern is 'Simplified' if it has run through the simplifier.

The simplifier runs until we do not know how to simplify a pattern any more. A
pattern 'isSimplified' if re-applying the simplifier would return the same
pattern.

Most patterns are assumed un-simplified until marked otherwise, so the
simplified status is reset by any substitution under the pattern.
-}
data Simplified
    = Simplified !SimplifiedData
    | NotSimplified
    deriving (Eq, GHC.Generic, Ord, Show)

pattern Simplified_ :: Type -> Condition -> Simplified
pattern Simplified_ sType condition =
    (Simplified SimplifiedData { sType, condition })

{-# COMPLETE Simplified_, NotSimplified #-}

instance Semigroup Simplified
  where
    NotSimplified <> _ = NotSimplified
    _ <> NotSimplified = NotSimplified

    (Simplified_ t1 c1) <> (Simplified_ t2 c2) =
        Simplified_ (t1 <> t2) (c1 <> c2)

instance Monoid Simplified where
    mempty = Simplified_ mempty mempty

instance SOP.Generic Simplified

instance SOP.HasDatatypeInfo Simplified

instance Debug Simplified

instance Diff Simplified where
    diffPrec = diffPrecIgnore

instance NFData Simplified

instance Hashable Simplified

{- | Computes the 'Simplified' attribute for a pattern given its default
attribute (usually a merge of the pattern's subterm simplification attributes)
and the desired one.

As an example, let us assume that the default attribute is
@Simplified (Partly, Condition c)@ and that we would want the attribute to be
@Simplified (Fully, Any)@.

Then let us notice that the term needs the condition @c@ (most likely because
one of its subterms is simplified only with it as a side condition), and that
the term and its subterms went through the simplifier (the 'Partly' tag), so
it's valid to mark it as fully simplified. The result will be
"Simplified (Fully, Condition c)".
-}
simplifiedTo
    :: HasCallStack
    => Simplified
    -- ^ Default value
    -> Simplified
    -- ^ Desired state
    -> Simplified
NotSimplified `simplifiedTo` NotSimplified = NotSimplified
_ `simplifiedTo` NotSimplified =
    error "Should not make sense to upgrade something else to NotSimplified."
NotSimplified `simplifiedTo` _ =
    error "Cannot upgrade NotSimplified to something else."

Simplified_ _ _       `simplifiedTo` s@(Simplified_ Fully Unknown) = s
Simplified_ _ Unknown `simplifiedTo`    Simplified_ Fully _ =
    Simplified_ Fully Unknown

Simplified_ _ (Condition c1) `simplifiedTo` s@(Simplified_ Fully (Condition c2))
  = if c1 == c2
    then s
    else Simplified_ Fully Unknown
Simplified_ _ Any `simplifiedTo` s@(Simplified_ Fully (Condition _)) = s

Simplified_ _ c@(Condition _) `simplifiedTo` Simplified_ Fully Any =
    Simplified_ Fully c
Simplified_ _ Any `simplifiedTo` s@(Simplified_ Fully Any) = s

s1@(Simplified_ _ _) `simplifiedTo` s2@(Simplified_ Partly _) = s1 <> s2

isSimplified :: SideCondition.Representation -> Simplified -> Bool
isSimplified _ (Simplified_ Fully Any) = True
isSimplified currentCondition (Simplified_ Fully (Condition condition)) =
    currentCondition == condition
isSimplified _ (Simplified_ Fully Unknown) = False
isSimplified _ (Simplified_ Partly _) = False
isSimplified _ NotSimplified = False

isFullySimplified :: Simplified -> Bool
isFullySimplified (Simplified_ Fully Any) = True
isFullySimplified (Simplified_ Fully (Condition _)) = False
isFullySimplified (Simplified_ Fully Unknown) = False
isFullySimplified (Simplified_ Partly _) = False
isFullySimplified NotSimplified = False

fullySimplified :: Simplified
fullySimplified = Simplified_ Fully Any

simplifiedConditionally :: SideCondition.Representation -> Simplified
simplifiedConditionally c = Simplified_ Fully (Condition c)

simplifiableConditionally :: SideCondition.Representation -> Simplified
simplifiableConditionally c = Simplified_ Partly (Condition c)

alwaysSimplified :: a -> Simplified
alwaysSimplified = const fullySimplified
{-# INLINE alwaysSimplified #-}

notSimplified :: Foldable a => a Simplified -> Simplified
notSimplified a
  | Foldable.null a = NotSimplified
  | otherwise = Foldable.fold a <> Simplified_ Partly Any
{-# INLINE notSimplified #-}

{- | Provides a short and incomplete textual description of a 'Simplified'
object, suitable for use as an explanatory comment when unparsing patterns.

There is no tag for "NotSimplified", since that's the default state.

Otherwise, the tag starts with a prefix that should be unique among all
attributes that have tags in order to prevent confusion ("S"), followed
by short representations of the 'Type' and 'Condition'.
-}
unparseTag :: Simplified -> Maybe Text
unparseTag (Simplified_ ty condition) =
    Just $ "S" <> typeRepresentation ty <> conditionRepresentation condition
  where
    typeRepresentation Fully = "f"
    typeRepresentation Partly = "p"

    conditionRepresentation Any = "a"
    conditionRepresentation (Condition _) = "c"
    conditionRepresentation Unknown = "u"
unparseTag NotSimplified = Nothing

instance Synthetic Simplified (Bottom sort) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Top sort) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const StringLiteral) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const InternalBytes) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Const (SomeVariable variable)) where
    synthetic = alwaysSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Exists sort variable) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Forall sort variable) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (And sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Or sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Not sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Application head) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Ceil sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Floor sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (DomainValue sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Equals sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (In sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Implies sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Iff sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Mu variable) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Nu variable) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Next sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Rewrites sort) where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified (Builtin key) where
    synthetic (BuiltinInt    _) = fullySimplified
    synthetic (BuiltinBool   _) = fullySimplified
    synthetic (BuiltinString _) = fullySimplified
    synthetic b@(BuiltinMap    _) = notSimplified b
    synthetic b@(BuiltinList   _) = notSimplified b
    synthetic b@(BuiltinSet    _) = notSimplified b
    {-# INLINE synthetic #-}

instance Synthetic Simplified Inhabitant where
    synthetic = notSimplified
    {-# INLINE synthetic #-}

instance Synthetic Simplified Inj where
    synthetic = synthetic . Inj.toApplication
    {-# INLINE synthetic #-}
