{- |
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

 -}

{-# LANGUAGE UndecidableInstances #-}

module Kore.Attribute.Pattern
    ( Pattern (Pattern, patternSort, freeVariables, functional, function, defined, created)
    -- 'simplified' and 'constructorLike' were intentionally left out above.
    , mapVariables
    , traverseVariables
    , deleteFreeVariable
    , isFullySimplified
    , isSimplified
    , setSimplified
    , simplifiedAttribute
    , constructorLikeAttribute
    -- * Re-exports
    , module Kore.Attribute.Pattern.Created
    , module Kore.Attribute.Pattern.Defined
    , module Kore.Attribute.Pattern.FreeVariables
    , module Kore.Attribute.Pattern.Function
    , module Kore.Attribute.Pattern.Functional
    , module Kore.Attribute.Pattern.Simplified
    ) where

import Prelude.Kore

import Control.DeepSeq
    ( NFData
    )
import qualified Control.Lens as Lens
import Data.Generics.Product
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Pattern.ConstructorLike
import Kore.Attribute.Pattern.Created
import Kore.Attribute.Pattern.Defined
import Kore.Attribute.Pattern.FreeVariables hiding
    ( freeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
    ( freeVariables
    )
import Kore.Attribute.Pattern.Function
import Kore.Attribute.Pattern.Functional
import Kore.Attribute.Pattern.Simplified hiding
    ( isFullySimplified
    , isSimplified
    )
import qualified Kore.Attribute.Pattern.Simplified as Simplified
    ( isFullySimplified
    , isSimplified
    )
import Kore.Attribute.Synthetic
import Kore.Debug
import qualified Kore.Internal.SideCondition.SideCondition as SideCondition
    ( Representation
    )
import Kore.Sort
    ( Sort
    )
import Kore.Syntax.Variable

{- | @Pattern@ are the attributes of a pattern collected during verification.
 -}
data Pattern variable =
    Pattern
        { patternSort :: !Sort
        -- ^ The sort determined by the verifier.
        , freeVariables :: !(FreeVariables variable)
        -- ^ The free variables of the pattern.
        , functional :: !Functional
        , function :: !Function
        , defined :: !Defined
        , created :: !Created
        , simplified :: !Simplified
        , constructorLike :: !ConstructorLike
        }
    deriving (Eq, GHC.Generic, Show)

-- TODO (thomas.tuegel): Lift 'simplified' to the 'Conditional' level once we
-- treat the latter as an aggregate root.

instance NFData variable => NFData (Pattern variable)

instance Hashable variable => Hashable (Pattern variable)

instance SOP.Generic (Pattern variable)

instance SOP.HasDatatypeInfo (Pattern variable)

instance Debug variable => Debug (Pattern variable) where
    debugPrecBrief _ _ = "_"

instance (Debug variable, Diff variable) => Diff (Pattern variable)

instance
    ( Synthetic Sort base
    , Synthetic (FreeVariables variable) base
    , Synthetic Functional base
    , Synthetic Function base
    , Synthetic Defined base
    , Synthetic Simplified base
    , Synthetic ConstructorLike base
    ) =>
    Synthetic (Pattern variable) base
  where
    synthetic base =
        Pattern
            { patternSort = synthetic (patternSort <$> base)
            , freeVariables = synthetic (freeVariables <$> base)
            , functional = synthetic (functional <$> base)
            , function = synthetic (function <$> base)
            , defined = synthetic (defined <$> base)
            , created = synthetic (created <$> base)
            , simplified =
                if isConstructorLike constructorLikeAttr
                    then fullySimplified
                    else synthetic (simplified <$> base)
            , constructorLike = constructorLikeAttr
            }
      where
        constructorLikeAttr :: ConstructorLike
        constructorLikeAttr = synthetic (constructorLike <$> base)

instance HasConstructorLike (Pattern variable) where
    extractConstructorLike
        Pattern {constructorLike}
      = constructorLike

simplifiedAttribute :: HasCallStack => Pattern variable -> Simplified
simplifiedAttribute patt@Pattern {simplified} =
    assertSimplifiedConsistency patt simplified

constructorLikeAttribute :: Pattern variable -> ConstructorLike
constructorLikeAttribute Pattern {constructorLike} = constructorLike

{- Checks whether the pattern is simplified relative to the given side
condition.
-}
isSimplified
    :: HasCallStack
    => SideCondition.Representation -> Pattern variable -> Bool
isSimplified sideCondition patt@Pattern {simplified} =
    assertSimplifiedConsistency patt
    $ Simplified.isSimplified sideCondition simplified

{- Checks whether the pattern is simplified relative to any side condition.
-}
isFullySimplified :: HasCallStack => Pattern variable -> Bool
isFullySimplified patt@Pattern {simplified} =
    assertSimplifiedConsistency patt
    $ Simplified.isFullySimplified simplified

assertSimplifiedConsistency :: HasCallStack => Pattern variable -> a -> a
assertSimplifiedConsistency Pattern {constructorLike, simplified}
  | isConstructorLike constructorLike
  , not (Simplified.isFullySimplified simplified) =
    error "Inconsistent attributes, constructorLike implies fully simplified."
  | otherwise = id

setSimplified :: Simplified -> Pattern variable -> Pattern variable
setSimplified simplified patt = patt { simplified }

{- | Use the provided mapping to replace all variables in a 'Pattern'.

See also: 'traverseVariables'

 -}
mapVariables
    :: Ord variable2
    => AdjSomeVariableName (variable1 -> variable2)
    -> Pattern variable1
    -> Pattern variable2
mapVariables adj = Lens.over (field @"freeVariables") (mapFreeVariables adj)

{- | Use the provided traversal to replace the free variables in a 'Pattern'.

See also: 'mapVariables'

 -}
traverseVariables
    :: forall m variable1 variable2
    .  Monad m
    => Ord variable2
    => AdjSomeVariableName (variable1 -> m variable2)
    -> Pattern variable1
    -> m (Pattern variable2)
traverseVariables adj = field @"freeVariables" (traverseFreeVariables adj)

{- | Delete the given variable from the set of free variables.
 -}
deleteFreeVariable
    :: Ord variable
    => SomeVariable variable
    -> Pattern variable
    -> Pattern variable
deleteFreeVariable variable =
    Lens.over (field @"freeVariables") (bindVariable variable)


instance HasFreeVariables (Pattern variable) variable where
    freeVariables = freeVariables
