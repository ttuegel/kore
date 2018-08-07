module Data.Annotation where

import Control.DeepSeq
       ( NFData )
import Control.Lens.Wrapped
       ( Wrapped (..) )
import Data.Hashable
       ( Hashable (..) )
import GHC.Generics
       ( Generic )

{- | Annotations of type @ann@.

Annotations are metadata that should not be considered for 'Eq' and
'Ord'. Therefore, all annotations compare as equal.

-}
newtype Annotation ann = Annotation { getAnnotation :: ann }
  deriving (Foldable, Functor, Generic, Read, Show, Traversable)

instance Eq (Annotation ann) where
    (==) _ _ = True

-- | TODO (thomas.tuegel): Should 'Hashable' consider annotations?
instance Hashable ann => Hashable (Annotation ann)

instance NFData ann => NFData (Annotation ann)

instance Ord (Annotation ann) where
    compare _ _ = EQ

instance Wrapped (Annotation ann) where
    type Unwrapped (Annotation ann) = ann
