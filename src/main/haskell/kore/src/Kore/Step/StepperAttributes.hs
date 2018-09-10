{-|
Module      : Kore.Step.StepperAttributes
Description : Attributes used for step execution
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.StepperAttributes
  ( StepperAttributes (..)
  , functionalAttribute
  , functionAttribute
  , constructorAttribute
  , injectiveAttribute
  , sortInjectionAttribute
  , hookAttribute
  ) where

import Data.Default

import           Kore.AST.Kore
                 ( CommonKorePattern )
import           Kore.Attribute.Parser
                 ( ParseAttributes (..) )
import qualified Kore.Attribute.Parser as Attribute
import           Kore.Builtin.Hook
import           Kore.Implicit.Attributes
                 ( keyOnlyAttribute )

{- | @constructorAttribute@ represents a @constructor@ attribute Kore pattern.

  Kore syntax:
  @
    constructor{}()
  @

 -}
constructorAttribute :: CommonKorePattern
constructorAttribute = keyOnlyAttribute "constructor"

{- | @injectiveAttribute@ represents a @injective@ attribute Kore pattern.

  Kore syntax:
  @
    injective{}()
  @

 -}
injectiveAttribute :: CommonKorePattern
injectiveAttribute = keyOnlyAttribute "injective"

{- | @functionAttribute@ represents a @function@ attribute Kore pattern.

  Kore syntax:
  @
    function{}()
  @

 -}
functionAttribute :: CommonKorePattern
functionAttribute    = keyOnlyAttribute "function"

{- | @functionalAttribute@ represents a @functional@ attribute Kore pattern.

  Kore syntax:
  @
    functional{}()
  @

 -}
functionalAttribute :: CommonKorePattern
functionalAttribute  = keyOnlyAttribute "functional"

{- | @sortInjectionAttribute@ represents a @sortInjection@ attribute Kore pattern.

  Kore syntax:
  @
    sortInjection{}()
  @

 -}
sortInjectionAttribute :: CommonKorePattern
sortInjectionAttribute  = keyOnlyAttribute "sortInjection"

-- |Data-structure containing attributes relevant to the Kore Stepper
data StepperAttributes =
    StepperAttributes
    { isFunction      :: !Bool
      -- ^ Whether a symbol represents a function
    , isFunctional    :: !Bool
      -- ^ Whether a symbol is functional
    , isConstructor   :: !Bool
      -- ^ Whether a symbol represents a constructor
    , isInjective     :: !Bool
      -- ^ Whether a symbol represents an injective function
    , isSortInjection :: !Bool
      -- ^ Whether a symbol is a sort injection
    , hook            :: !Hook
      -- ^ The builtin sort or symbol hooked to a sort or symbol
    }
  deriving (Eq, Show)

defaultStepperAttributes :: StepperAttributes
defaultStepperAttributes =
    StepperAttributes
    { isFunction       = False
    , isFunctional     = False
    , isConstructor    = False
    , isInjective      = False
    , isSortInjection  = False
    , hook             = def
    }

-- | See also: 'defaultStepperAttributes'
instance Default StepperAttributes where
    def = defaultStepperAttributes

{- | Is the @functional@ Kore attribute present?

  It is a parse error if the @functional@ attribute is given any arguments.

  See also: 'functionalAttribute'

 -}
hasFunctionalAttribute :: Attribute.Parser Bool
hasFunctionalAttribute = Attribute.hasKeyOnlyAttribute "functional"

{- | Is the @function@ Kore attribute present?

  It is a parse error if the @function@ attribute is given any arguments.

  See also: 'functionAttribute'

 -}
hasFunctionAttribute :: Attribute.Parser Bool
hasFunctionAttribute = Attribute.hasKeyOnlyAttribute "function"

{- | Is the @constructor@ Kore attribute present?

  It is a parse error if the @constructor@ attribute is given any arguments.

  See also: 'constructorAttribute'

 -}
hasConstructorAttribute :: Attribute.Parser Bool
hasConstructorAttribute = Attribute.hasKeyOnlyAttribute "constructor"

{- | Is the @injective@ Kore attribute present?

  It is a parse error if the @injective@ attribute is given any arguments.

  See also: 'injectiveAttribute'

 -}
hasInjectiveAttribute :: Attribute.Parser Bool
hasInjectiveAttribute = Attribute.hasKeyOnlyAttribute "injective"

{- | Is the @sortInjection@ Kore attribute present?

  It is a parse error if the @sortInjection@ attribute is given any arguments.

  See also: 'sortInjectionAttribute'

 -}
hasSortInjectionAttribute :: Attribute.Parser Bool
hasSortInjectionAttribute = Attribute.hasKeyOnlyAttribute "sortInjection"

instance ParseAttributes StepperAttributes where
    attributesParser =
        do
        isFunctional <- hasFunctionalAttribute
        isFunction <- hasFunctionAttribute
        isConstructor <- hasConstructorAttribute
        isSortInjection <- hasSortInjectionAttribute
        isInjective <-
            ((isConstructor || isSortInjection) ||) <$> hasInjectiveAttribute
        hook <- attributesParser
        pure StepperAttributes
            { isFunction
            , isFunctional
            , isConstructor
            , isSortInjection
            , isInjective
            , hook
            }
