{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

module Kore.Attribute.Axiom.Concrete
    ( Concrete (..), isConcreteVariable, concreteVariable
    , concreteId, concreteSymbol, concreteAttribute
    , mapConcreteVariables
    , parseConcreteAttribute
    , parseFreeVariables -- used by Symbolic
    -- * Re-exports
    , FreeVariables
    , NamedVariable
    ) where

import Prelude.Kore

import qualified Control.Monad as Monad
import Data.Set
    ( Set
    )
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Attribute.Parser as Parser
import Kore.Attribute.Pattern.FreeVariables
    ( FreeVariables
    , NamedVariable
    , freeVariable
    , isFreeVariable
    , mapFreeVariables
    )
import qualified Kore.Attribute.Pattern.FreeVariables as FreeVariables
import Kore.Debug
import qualified Kore.Error
import Kore.Syntax.ElementVariable
import Kore.Syntax.SetVariable
import Kore.Syntax.Variable
    ( Variable
    )
import Kore.Unparser
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable
    )
import qualified Pretty

{- | @Concrete@ represents the @concrete@ attribute for axioms.
 -}
newtype Concrete variable =
    Concrete { unConcrete :: FreeVariables variable }
    deriving (Eq, GHC.Generic, Ord, Show, Semigroup, Monoid)

instance SOP.Generic (Concrete variable)

instance SOP.HasDatatypeInfo (Concrete variable)

instance Debug variable => Debug (Concrete variable)

instance (Debug variable, Diff variable) => Diff (Concrete variable)

instance NFData variable => NFData (Concrete variable)

instance NamedVariable variable => Default (Concrete variable) where
    def = Concrete mempty

instance From (Concrete variable) (Set (UnifiedVariable variable)) where
    from = from @(FreeVariables _) . unConcrete
    {-# INLINE from #-}

isConcreteVariable
    :: UnifiedVariable variable
    -> Concrete variable
    -> Bool
isConcreteVariable variable (Concrete freeVariables) =
    FreeVariables.isFreeVariable variable freeVariables

concreteVariable
    :: NamedVariable variable
    => UnifiedVariable variable
    -> Concrete variable
concreteVariable = Concrete . FreeVariables.freeVariable

-- | Kore identifier representing the @concrete@ attribute symbol.
concreteId :: Id
concreteId = "concrete"

-- | Kore symbol representing the @concrete@ attribute.
concreteSymbol :: SymbolOrAlias
concreteSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = concreteId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @concrete@ attribute.
concreteAttribute :: [UnifiedVariable Variable] -> AttributePattern
concreteAttribute = attributePattern concreteSymbol . map attributeVariable

parseConcreteAttribute
    :: FreeVariables Variable
    -> AttributePattern
    -> Concrete Variable
    -> Parser (Concrete Variable)
parseConcreteAttribute freeVariables =
    Parser.withApplication concreteId parseApplication
  where
    parseApplication params args (Concrete concreteVars) =
        Concrete <$> parseFreeVariables freeVariables params args concreteVars

parseFreeVariables
    :: FreeVariables Variable
    -> [Sort]
    -> [AttributePattern]
    -> FreeVariables Variable
    -> Parser (FreeVariables Variable)
parseFreeVariables freeVariables params args concretes = do
    Parser.getZeroParams params
    (duplicates, concretes') <-
        Monad.foldM addVariable ([], concretes)
        .   defaultFreeVariables
        =<< mapM getVariable args
    unless (null duplicates) $
        (Kore.Error.koreFail . show . Pretty.vsep)
            ( "duplicate concrete/symbolic variable annotations for:"
            : (Pretty.indent 4 . unparse <$> duplicates)
            )
    return concretes'
  where
    -- If no explicit arguments are given, use all the FreeVariables.
    defaultFreeVariables
        :: [UnifiedVariable Variable] -> [UnifiedVariable Variable]
    defaultFreeVariables [] = FreeVariables.toList freeVariables
    defaultFreeVariables xs = xs

    addVariable
        :: ([UnifiedVariable Variable], FreeVariables Variable)
        -> UnifiedVariable Variable
        -> Parser ([UnifiedVariable Variable], FreeVariables Variable)
    addVariable (duplicates, concreteVariables) variable
      | not (isFreeVariable variable freeVariables) =
        (Kore.Error.koreFail . show . Pretty.hsep)
            [ "expected free variable, but found:"
            , unparse variable
            ]
      | isFreeVariable variable concreteVariables =
        pure (variable : duplicates, concreteVariables)
      | otherwise =
        pure (duplicates, concreteVariables')
      where
        concreteVariables' = freeVariable variable <> concreteVariables

instance From (Concrete Variable) Attributes where
    from =
        from @AttributePattern
        . concreteAttribute
        . FreeVariables.toList
        . unConcrete

mapConcreteVariables
    :: NamedVariable variable2
    => (ElementVariable variable1 -> ElementVariable variable2)
    -> (SetVariable variable1 -> SetVariable variable2)
    ->  Concrete variable1 -> Concrete variable2
mapConcreteVariables mapElemVar mapSetVar (Concrete freeVariables) =
    Concrete (mapFreeVariables mapElemVar mapSetVar freeVariables)
