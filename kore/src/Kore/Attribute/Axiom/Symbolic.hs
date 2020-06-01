{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA
 -}
module Kore.Attribute.Axiom.Symbolic
    ( Symbolic (..), isSymbolic
    , symbolicId, symbolicSymbol, symbolicAttribute
    , mapSymbolicVariables
    , parseSymbolicAttribute
    -- * Re-exports
    , FreeVariables
    ) where

import Prelude.Kore

import Data.Set
    ( Set
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Axiom.Concrete
    ( parseFreeVariables
    )
import Kore.Attribute.Parser as Parser
import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    , isFreeVariable
    , mapFreeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import Kore.Debug
import Kore.Syntax.Variable

{- | @Symbolic@ represents the @symbolic@ attribute for axioms.
 -}
newtype Symbolic variable =
    Symbolic { unSymbolic :: FreeVariables variable }
    deriving (Eq, GHC.Generic, Ord, Show, Semigroup, Monoid)

instance SOP.Generic (Symbolic variable)

instance SOP.HasDatatypeInfo (Symbolic variable)

instance Debug variable => Debug (Symbolic variable)

instance (Debug variable, Diff variable) => Diff (Symbolic variable)

instance NFData variable => NFData (Symbolic variable)

instance Ord variable => Default (Symbolic variable) where
    def = Symbolic mempty

instance
    Ord variable => From (Symbolic variable) (Set (SomeVariable1 variable))
  where
    from = from @(FreeVariables _) . unSymbolic
    {-# INLINE from #-}

instance From (Symbolic variable) (Set (SomeVariableName variable)) where
    from = from @(FreeVariables _) . unSymbolic
    {-# INLINE from #-}

-- | Kore identifier representing the @symbolic@ attribute symbol.
symbolicId :: Id
symbolicId = "symbolic"

-- | Kore symbol representing the @symbolic@ attribute.
symbolicSymbol :: SymbolOrAlias
symbolicSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = symbolicId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @symbolic@ attribute.
symbolicAttribute :: [SomeVariable1 VariableName] -> AttributePattern
symbolicAttribute = attributePattern symbolicSymbol . map attributeVariable

parseSymbolicAttribute
    :: FreeVariables VariableName
    -> AttributePattern
    -> Symbolic VariableName
    -> Parser (Symbolic VariableName)
parseSymbolicAttribute freeVariables =
    Parser.withApplication symbolicId parseApplication
  where
    parseApplication params args (Symbolic concreteVars) =
        Symbolic <$> parseFreeVariables freeVariables params args concreteVars

instance From (Symbolic VariableName) Attributes where
    from =
        from @AttributePattern @Attributes
        . symbolicAttribute
        . FreeVariables.toList
        . unSymbolic

mapSymbolicVariables
    :: Ord variable2
    => AdjSomeVariableName (variable1 -> variable2)
    -> Symbolic variable1
    -> Symbolic variable2
mapSymbolicVariables adj (Symbolic freeVariables) =
    Symbolic (mapFreeVariables adj freeVariables)

isSymbolic
    :: Ord variable
    => Symbolic variable
    -> SomeVariableName variable
    -> Bool
isSymbolic (Symbolic vars) = not . flip isFreeVariable vars
