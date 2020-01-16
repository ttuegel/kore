module Kore.Step.Simplification.Ceil
    ( makeEvaluate
    , makeEvaluateTerm
    ) where

import Kore.Internal.OrCondition
    ( OrCondition
    )
import Kore.Internal.OrPattern
    ( OrPattern
    )
import Kore.Internal.Pattern
    ( Pattern
    )
import Kore.Internal.SideCondition
    ( SideCondition
    )
import Kore.Internal.TermLike
    ( TermLike
    )
import Kore.Step.Simplification.Simplify
    ( MonadSimplify
    , SimplifierVariable
    )

makeEvaluate
    :: SimplifierVariable variable
    => MonadSimplify simplifier
    => SideCondition variable
    -> Pattern variable
    -> simplifier (OrPattern variable)

makeEvaluateTerm
    :: forall variable simplifier
    .  SimplifierVariable variable
    => MonadSimplify simplifier
    => SideCondition variable
    -> TermLike variable
    -> simplifier (OrCondition variable)
