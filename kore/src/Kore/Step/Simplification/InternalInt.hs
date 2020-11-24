{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
-}
module Kore.Step.Simplification.InternalInt
    ( simplify
    ) where

import Prelude.Kore

import Kore.Internal.InternalInt
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.TermLike

simplify
    :: InternalVariable variable
    => InternalInt
    -> OrPattern variable
simplify = OrPattern.fromPattern . pure . mkInternalInt
