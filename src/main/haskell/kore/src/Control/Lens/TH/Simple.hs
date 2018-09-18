{-|
Module      : Control.Lens.TH.Simple
Description : The simplest possible lens rules
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

'simpleRules' is a rule for naming lenses which is even simpler than the
simplest rule in "Control.Lens.TH": it generates lenses with the same names as
the corresponding fields. Use 'makeSimpleLenses' or @'makeLensesWith'
'simpleRules'@ to apply this simple rule.

-}

module Control.Lens.TH.Simple
    ( simpleRules
    , makeSimpleLenses
    ) where

import Control.Lens
import Language.Haskell.TH
       ( DecsQ )
import Language.Haskell.TH.Syntax
       ( Name (..), NameFlavour (..) )

simpleNamer :: FieldNamer
simpleNamer _ _ (Name occName _) = [TopName (Name occName NameS)]

{- | The simplest possible lens rule: make lenses with the same names as fields.
 -}
simpleRules :: LensRules
simpleRules = lensRules & lensField .~ simpleNamer

{- | Make lenses using 'simpleRules'.
 -}
makeSimpleLenses
    :: Name  -- ^ type name
    -> DecsQ
makeSimpleLenses = makeLensesWith simpleRules
