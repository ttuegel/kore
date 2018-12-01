module Kore.Step.Pattern
    ( StepPattern
    , CommonStepPattern
    , ConcreteStepPattern
    , StepPatternHead
    , module Kore.AST.Pure
    , module Kore.Annotation.Valid
    ) where

import           Kore.Annotation.Valid
import           Kore.AST.Pure
                 ( Concrete, Pattern, PurePattern, Variable )
import qualified Kore.Domain.Builtin as Domain

type StepPattern lvl var = PurePattern lvl Domain.Builtin var (Valid lvl)

type CommonStepPattern lvl = StepPattern lvl Variable

type ConcreteStepPattern lvl = StepPattern lvl Concrete

type StepPatternHead lvl var = Pattern lvl Domain.Builtin var
