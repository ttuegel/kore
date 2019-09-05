{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
-}
module Kore.Step.Simplification.Mu
    ( simplify
    , makeEvaluate
    ) where

import Kore.Internal.OrPattern
       ( OrPattern )
import Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike
import Kore.Step.Simplification.Data
       ( SimplifierVariable )

{-|'simplify' simplifies a 'Mu' pattern with an 'OrPattern'
child.
-}
simplify
    :: SimplifierVariable variable
    => Mu variable (OrPattern variable)
    -> OrPattern variable
simplify
    Mu { muVariable, muChild }
  = makeEvaluate muVariable <$> muChild

{-| evaluates a 'Mu' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate
    :: SimplifierVariable variable
    => SetVariable variable
    -> Pattern variable
    -> Pattern variable
makeEvaluate variable patt =
    Pattern.fromTermLike $ mkMu variable $ Pattern.toTermLike patt
