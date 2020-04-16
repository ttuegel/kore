{- |
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
 -}
module Kore.Variables.Fresh
    ( FreshPartialOrd (..)
    , FreshVariable (..)
    , refreshVariables
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

import Data.Sup
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

{- | A @FreshVariable@ can be renamed to avoid colliding with a set of names.
-}
class Ord variable => FreshVariable variable where
    {- | Refresh a variable, renaming it avoid the given set.

    If the given variable occurs in the set, @refreshVariable@ must return
    'Just' a fresh variable which does not occur in the set. If the given
    variable does /not/ occur in the set, @refreshVariable@ /may/ return
    'Nothing'.

     -}
    refreshVariable
        :: Set variable  -- ^ variables to avoid
        -> variable      -- ^ variable to rename
        -> Maybe variable
    default refreshVariable
        :: (FreshPartialOrd variable, SortedVariable variable)
        => Set variable
        -> variable
        -> Maybe variable
    refreshVariable avoiding original = do
        largest <- assignSort <$> Set.lookupLT (supVariable original) avoiding
        Monad.guard (largest >= infVariable original)
        pure (nextVariable largest)
      where
        originalSort = Lens.view lensVariableSort original
        assignSort = Lens.set lensVariableSort originalSort
    {-# INLINE refreshVariable #-}

instance
    (FreshPartialOrd variable, SortedVariable variable)
    => FreshVariable (ElementVariable variable)

instance
    (FreshPartialOrd variable, SortedVariable variable)
    => FreshVariable (SetVariable variable)

instance FreshVariable Variable

instance FreshVariable Concrete where
    refreshVariable _ = \case {}

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
    :: FreshVariable variable
    => Set variable  -- ^ variables to avoid
    -> Set variable  -- ^ variables to rename
    -> Map variable variable
refreshVariables avoid0 =
    snd <$> Foldable.foldl' refreshVariablesWorker (avoid0, Map.empty)
  where
    refreshVariablesWorker (avoid, rename) var
      | Just var' <- refreshVariable avoid var =
        let avoid' =
                -- Avoid the freshly-generated variable in future renamings.
                Set.insert var' avoid
            rename' =
                -- Record a mapping from the original variable to the
                -- freshly-generated variable.
                Map.insert var var' rename
        in (avoid', rename')
      | otherwise =
        -- The variable does not collide with any others, so renaming is not
        -- necessary.
        (Set.insert var avoid, rename)
