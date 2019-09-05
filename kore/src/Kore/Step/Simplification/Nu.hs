{-|
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
-}
module Kore.Step.Simplification.Nu
    ( simplify
    , makeEvaluate
    ) where

import Kore.Internal.OrPattern
       ( OrPattern )
import Kore.Internal.Pattern as Pattern
import Kore.Internal.TermLike
import Kore.Step.Simplification.Data
       ( SimplifierVariable )

{-|'simplify' simplifies a 'Nu' pattern with an 'OrPattern'
child.
-}
simplify
    :: SimplifierVariable variable
    => Nu variable (OrPattern variable)
    -> OrPattern variable
simplify
    Nu { nuVariable, nuChild }
  = makeEvaluate nuVariable <$> nuChild

{-| evaluates a 'Nu' given its two 'Pattern' children.

See 'simplify' for detailed documentation.
-}
makeEvaluate
    :: SimplifierVariable variable
    => SetVariable variable
    -> Pattern variable
    -> Pattern variable
makeEvaluate variable patt =
    Pattern.fromTermLike $ mkNu variable $ Pattern.toTermLike patt
