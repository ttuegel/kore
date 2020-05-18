{- |
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA

 -}

{-# LANGUAGE UndecidableInstances #-}

module Kore.Variables.Fresh
    ( FreshPartialOrd (..)
    , FreshVariable (..)
    , refreshVariables
    , Avoiding
    , avoid
    -- * Re-exports
    , module Kore.Syntax.Variable
    ) where

import Prelude.Kore

import qualified Control.Lens as Lens
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import Data.Generics.Product
    ( field
    )
import Data.Generics.Wrapped
    ( _Unwrapped
    )
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Set
    ( Set
    )
import qualified Data.Set as Set
import Data.Void
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Data.Sup
import Debug
import Kore.Syntax.ElementVariable
import Kore.Syntax.Id
import Kore.Syntax.SetVariable
import Kore.Syntax.Variable

{- | @FreshPartialOrder@ defines a partial order for renaming variables.

Two variables @x@ and @y@ are related under the partial order if @infVariable@
and @supVariable@ give the same value on @x@ and @y@.

Disjoint:

prop> infVariable x /= supVariable y

prop> (infVariable x == infVariable y) == (supVariable x == supVariable y)

Order:

prop> infVariable x <= x

prop> x <= supVariable x

prop> infVariable x < supVariable x

Idempotence:

prop> infVariable x == infVariable (infVariable x)

prop> supVariable x == supVariable (supVariable x)

Monotonicity:

prop> x < supVariable x ==> x < nextVariable x

Bounding:

prop> x < supVariable x ==> infVariable x < nextVariable x

prop> x < supVariable x ==> nextVariable x < supVariable x

 -}
class Ord variable => FreshPartialOrd variable where
    infVariable :: variable -> variable

    {- | @supVariable x@ is the greatest variable related to @x@.

    In the typical implementation, the counter has type
    @'Maybe' ('Sup' 'Natural')@
    so that @supVariable x@ has a counter @'Just' 'Sup'@.

     -}
    supVariable :: variable -> variable

    {- | @nextVariable@ increments the counter attached to a variable.
     -}
    nextVariable :: variable -> variable

instance FreshPartialOrd Void where
    infVariable = \case {}
    supVariable = \case {}
    nextVariable = \case {}

instance FreshPartialOrd VariableName where
    infVariable variable = variable { counter = Nothing }
    {-# INLINE infVariable #-}

    supVariable variable = variable { counter = Just Sup }
    {-# INLINE supVariable #-}

    nextVariable =
        Lens.over (field @"counter") incrementCounter
        . Lens.set (field @"base" . field @"idLocation") generated
      where
        generated = AstLocationGeneratedVariable
        incrementCounter counter =
            case counter of
                Nothing          -> Just (Element 0)
                Just (Element n) -> Just (Element (succ n))
                Just Sup         -> illegalVariableCounter
    {-# INLINE nextVariable #-}

instance
    FreshPartialOrd variable => FreshPartialOrd (ElementVariableName variable)
  where
    infVariable = fmap infVariable
    {-# INLINE infVariable #-}

    supVariable = fmap supVariable
    {-# INLINE supVariable #-}

    nextVariable = fmap nextVariable
    {-# INLINE nextVariable #-}

instance
    FreshPartialOrd variable => FreshPartialOrd (SetVariableName variable)
  where
    infVariable = fmap infVariable
    {-# INLINE infVariable #-}

    supVariable = fmap supVariable
    {-# INLINE supVariable #-}

    nextVariable = fmap nextVariable
    {-# INLINE nextVariable #-}

instance
    FreshPartialOrd variable => FreshPartialOrd (SomeVariableName variable)
  where
    infVariable = fmap infVariable
    {-# INLINE infVariable #-}

    supVariable = fmap supVariable
    {-# INLINE supVariable #-}

    nextVariable = fmap nextVariable
    {-# INLINE nextVariable #-}

instance FreshPartialOrd Variable where
    infVariable variable = variable { variableCounter = Nothing }
    {-# INLINE infVariable #-}

    supVariable variable = variable { variableCounter = Just Sup }
    {-# INLINE supVariable #-}

    nextVariable =
        Lens.over (field @"variableCounter") incrementCounter
        . Lens.set (field @"variableName" . field @"idLocation") generated
      where
        generated = AstLocationGeneratedVariable
        incrementCounter counter =
            case counter of
                Nothing          -> Just (Element 0)
                Just (Element n) -> Just (Element (succ n))
                Just Sup         -> illegalVariableCounter
    {-# INLINE nextVariable #-}

instance FreshPartialOrd Concrete where
    infVariable = \case {}
    supVariable = \case {}
    nextVariable = \case {}

instance
    FreshPartialOrd variable
    => FreshPartialOrd (ElementVariable variable)
  where
    infVariable = Lens.over _Unwrapped infVariable
    {-# INLINE infVariable #-}

    supVariable = Lens.over _Unwrapped supVariable
    {-# INLINE supVariable #-}

    nextVariable = Lens.over _Unwrapped nextVariable
    {-# INLINE nextVariable #-}

instance
    FreshPartialOrd variable
    => FreshPartialOrd (SetVariable variable)
  where
    infVariable = Lens.over _Unwrapped infVariable
    {-# INLINE infVariable #-}

    supVariable = Lens.over _Unwrapped supVariable
    {-# INLINE supVariable #-}

    nextVariable = Lens.over _Unwrapped nextVariable
    {-# INLINE nextVariable #-}

newtype Avoiding variable =
    Avoiding { getAvoiding :: Set variable }
    deriving (Eq, Ord, Show)
    deriving (GHC.Generic)

instance SOP.Generic (Avoiding variable)

instance SOP.HasDatatypeInfo (Avoiding variable)

instance Debug variable => Debug (Avoiding variable)

instance (Debug variable, Diff variable) => Diff (Avoiding variable)

instance Ord variable => Semigroup (Avoiding variable) where
    (<>) a b = Avoiding (on (<>) getAvoiding a b)
    {-# INLINE (<>) #-}

instance Ord variable => Monoid (Avoiding variable) where
    mempty = Avoiding mempty
    {-# INLINE mempty #-}

instance From (Set variable) (Avoiding variable) where
    from = Avoiding
    {-# INLINE from #-}

instance From (Avoiding variable) (Set variable) where
    from = getAvoiding
    {-# INLINE from #-}

avoid
    :: NamedVariable variable
    => variable
    -> Avoiding (VariableNameOf variable)
avoid = Avoiding . Set.singleton . Lens.view lensVariableName

{- | A @FreshVariable@ can be renamed to avoid colliding with a set of names.
-}
class Ord (VariableNameOf variable) => FreshVariable variable where
    {- | Refresh a variable, renaming it avoid the given set.

    If the given variable occurs in the set, @refreshVariable@ must return
    'Just' a fresh variable which does not occur in the set. If the given
    variable does /not/ occur in the set, @refreshVariable@ /may/ return
    'Nothing'.

     -}
    refreshVariable
        :: Avoiding (VariableNameOf variable)  -- ^ variables to avoid
        -> variable      -- ^ variable to rename
        -> Maybe variable
    default refreshVariable
        :: (FreshPartialOrd (VariableNameOf variable), NamedVariable variable)
        => Avoiding (VariableNameOf variable)
        -> variable
        -> Maybe variable
    refreshVariable = defaultRefreshVariable
    {-# INLINE refreshVariable #-}

    refreshVariableName
        :: Set (VariableNameOf variable)  -- ^ variables to avoid
        -> variable      -- ^ variable to rename
        -> Maybe variable
    default refreshVariableName
        :: (FreshPartialOrd (VariableNameOf variable), NamedVariable variable)
        => Set (VariableNameOf variable)
        -> variable
        -> Maybe variable
    refreshVariableName = defaultRefreshVariableName

defaultRefreshVariable
    :: FreshPartialOrd (VariableNameOf variable)
    => NamedVariable variable
    => Avoiding (VariableNameOf variable)
    -> variable
    -> Maybe variable
defaultRefreshVariable avoiding =
    getAvoiding avoiding
    & defaultRefreshVariableName
{-# INLINE defaultRefreshVariable #-}

defaultRefreshVariableName
    :: FreshPartialOrd (VariableNameOf variable)
    => NamedVariable variable
    => Set (VariableNameOf variable)
    -> variable
    -> Maybe variable
defaultRefreshVariableName avoiding variable = do
    let original = Lens.view lensVariableName variable
    let sup = supVariable original
    largest <- Set.lookupLT sup avoiding
    Monad.guard (largest >= infVariable original)
    let next = nextVariable largest
    -- nextVariable must yield a variable greater than largest.
    assert (next > largest) $ pure $ Lens.set lensVariableName next variable
{-# INLINE defaultRefreshVariableName #-}

instance
    (FreshPartialOrd (VariableNameOf variable), NamedVariable variable)
    => FreshVariable (ElementVariable variable)

instance
    (FreshPartialOrd (VariableNameOf variable), NamedVariable variable)
    => FreshVariable (SetVariable variable)

instance FreshVariable Variable

instance FreshVariable Concrete where
    refreshVariable _ = \case {}
    refreshVariableName _ = \case {}

{- | Rename one set of variables while avoiding another.

If any of the variables to rename occurs in the set of avoided variables, it
will be mapped to a fresh name in the result. Every fresh name in the result
will also be unique among the fresh names.

To use @refreshVariables@ with 'Kore.Internal.Pattern.substitute', map the
result with 'Kore.Internal.TermLike.mkVar':

@
'Kore.Internal.TermLike.substitute'
    ('Kore.Internal.TermLike.mkVar' \<$\> refreshVariables avoid rename)
    :: 'Kore.Internal.TermLike.TermLike' Variable
    -> 'Kore.Internal.TermLike.TermLike' Variable
@

 -}
refreshVariables
    :: (FreshVariable variable, NamedVariable variable)
    => Avoiding (VariableNameOf variable)  -- ^ variables to avoid
    -> Set variable  -- ^ variables to rename
    -> Map variable variable
refreshVariables avoid0 =
    snd <$> Foldable.foldl' refreshVariablesWorker (avoid0, Map.empty)
  where
    refreshVariablesWorker (avoiding, rename) var
      | Just var' <- refreshVariable avoiding var =
        let avoiding' =
                -- Avoid the freshly-generated variable in future renamings.
                avoid var' <> avoiding
            rename' =
                -- Record a mapping from the original variable to the
                -- freshly-generated variable.
                Map.insert var var' rename
        in (avoiding', rename')
      | otherwise =
        -- The variable does not collide with any others, so renaming is not
        -- necessary.
        (avoid var <> avoiding, rename)
