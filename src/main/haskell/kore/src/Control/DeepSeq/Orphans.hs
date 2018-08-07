{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.DeepSeq.Orphans where

import Control.Comonad.Trans.Cofree
       ( Cofree, CofreeF (..) )
import Control.DeepSeq
       ( NFData (..) )
import Data.Functor.Compose
       ( Compose (..) )
import Data.Functor.Foldable
       ( Fix (..), project )
import Data.Functor.Identity
       ( Identity (..) )

instance NFData (f (Fix f)) => NFData (Fix f) where
    rnf (Fix f) = rnf f

instance
    (Functor f, NFData a, NFData (f (Cofree f a))) =>
    NFData (Cofree f a)
  where
    rnf (project -> Compose (Identity (a :< f))) = rnf a `seq` rnf f

instance (NFData a, NFData (f b)) => NFData (CofreeF f a b) where
    rnf (a :< fb) = rnf a `seq` rnf fb
