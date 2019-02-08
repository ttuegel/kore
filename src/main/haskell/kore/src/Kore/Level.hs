{-|
Module      : Kore.Level
Description : Meta- and object-level Kore
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

"Kore.Level" defines a representation of the meta- and object-levels as Haskell
types. 'SLevel' is a singleton witness of the promoted types.

See also:
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf Semantics of K>

-}
module Kore.Level
    ( Level (..)
    , SLevel (..)
    , IsLevel (..)
    , Unified (..)
    , applyUnified
    , transformUnified
    , mapUnified
    , sequenceUnified
    , asUnified
    , module Data.Kind
    ) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

import Data.Kind

{- | @Level@ indicates the @Meta@ or @Object@ level of Kore.

See also:
<http://github.com/kframework/kore/blob/master/docs/semantics-of-k.pdf Semantics of K>
 -}
data Level = Meta | Object

{- | A singleton witness of 'Level'.

Such a witness is called /singleton/ because it has exactly one inhabitant
(constructor) for each value of @Level@.

See also: 'Level'.

 -}
data SLevel :: Level -> Type where
    SMeta :: SLevel 'Meta
    SObject :: SLevel 'Object

{- | @IsLevel@ is used to pass 'Level' witnesses implicitly as constraints.

See also: 'Level', 'SLevel'

 -}
class IsLevel (level :: Level) where
    -- | Given a @thing@ parameterized by @level@, return a witness of @level@.
    isLevel :: forall thing. thing level -> SLevel level

instance IsLevel 'Meta where
    isLevel _ = SMeta
    {-# INLINE isLevel #-}

instance IsLevel 'Object where
    isLevel _ = SObject
    {-# INLINE isLevel #-}

{- | Generalize a 'Level'-parameterized @thing@ over all levels.
 -}
data Unified :: (Level -> Type) -> Type where
    UnifiedMeta :: thing 'Meta -> Unified thing
    UnifiedObject :: thing 'Object -> Unified thing

deriving instance (forall level. Eq (thing level)) => Eq (Unified thing)

deriving instance Generic (Unified thing)

deriving instance (forall level. Ord (thing level)) => Ord (Unified thing)

deriving instance (forall level. Show (thing level)) => Show (Unified thing)

instance (forall level. Hashable (thing level)) => Hashable (Unified thing)

instance (forall level. NFData (thing level)) => NFData (Unified thing)

{-|Given a function transforming objects of 'Meta' type and another transforming
objects of 'Object' type, 'applyUnified' builds the corresponding direct sum
function combining their effects to transform an 'Unified' object.
-}
applyUnified
    :: (thing 'Meta -> b)
    -> (thing 'Object -> b)
    -> (Unified thing -> b)
applyUnified metaT _ (UnifiedMeta x)     = metaT x
applyUnified _ objectT (UnifiedObject x) = objectT x

{-|Given a function transforming objects of any 'level', 'transformUnified'
"lifts" the function to apply on 'Unified' objects.
-}
transformUnified
    :: (forall level. thing level -> b)
    -> (Unified thing -> b)
transformUnified f = applyUnified f f

{-| Given a function transforming @thing1 level@ objects into @thing2 level@
ones, it builds one transforming 'Unified' @thing1@ objects into 'Unified'
@thing2@ ones.

Its functionality is akin fo that of 'Functor.fmap'
-}
mapUnified
    :: (forall level. thing1 level -> thing2 level)
    -> (Unified thing1 -> Unified thing2)
mapUnified f (UnifiedObject o) = UnifiedObject (f o)
mapUnified f (UnifiedMeta o)   = UnifiedMeta (f o)

{-|Given a function transforming @thing1 level@ objects into an action
producing @thing2 level@ objects,
it builds one transforming 'Unified' @thing1@ objects into
actions procuding 'Unified' @thing2@ objects.

Its functionality is akin fo that of 'Applicative.sequence'
-}
sequenceUnified
    :: Applicative f
    => (forall level. thing1 level -> f (thing2 level))
    -> (Unified thing1 -> f (Unified thing2))
sequenceUnified f (UnifiedObject o) = UnifiedObject <$> f o
sequenceUnified f (UnifiedMeta o)   = UnifiedMeta <$> f o

{-|'asUnified' takes an arbitrary 'Meta' or 'Object' @thing@ and transforms it
into the corresponding 'Unified' @thing@.
-}
asUnified :: IsLevel level => thing level -> Unified thing
asUnified x =
    case isLevel x of
        SObject -> UnifiedObject x
        SMeta   -> UnifiedMeta x
