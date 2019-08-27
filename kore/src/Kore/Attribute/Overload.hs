{-|
Module      : Kore.Attribute.Overload
Description : Overloaded production attribute
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

-}
module Kore.Attribute.Overload
    ( Overload (..)
    , overloadId, overloadSymbol, overloadAttribute
    ) where

import Kore.Attribute.Parser as Parser

-- | @Overload@ represents the @overload@ attribute for symbols.
newtype Overload =
    Overload
        { getOverload :: Maybe (SymbolOrAlias, SymbolOrAlias) }
    deriving (Generic, Eq, Ord, Show)

instance Semigroup Overload where
    (<>) a@(Overload (Just _)) _ = a
    (<>) _                     b = b

instance Monoid Overload where
    mempty = Overload { getOverload = Nothing }

instance Default Overload where
    def = mempty

instance NFData Overload

-- | Kore identifier representing the @overload@ attribute symbol.
overloadId :: Id
overloadId = "overload"

-- | Kore symbol representing the @overload@ attribute.
overloadSymbol :: SymbolOrAlias
overloadSymbol =
    SymbolOrAlias
        { symbolOrAliasConstructor = overloadId
        , symbolOrAliasParams = []
        }

-- | Kore pattern representing the @overload@ attribute.
overloadAttribute
    :: SymbolOrAlias
    -> SymbolOrAlias
    -> AttributePattern
overloadAttribute symbol1 symbol2 =
    attributePattern overloadSymbol
        [ attributePattern_ symbol1
        , attributePattern_ symbol2
        ]

instance ParseAttributes Overload where
    parseAttribute = withApplication' parseApplication
      where
        parseApplication params args Overload { getOverload }
          | Just _ <- getOverload = failDuplicate'
          | otherwise = do
            Parser.getZeroParams params
            (arg1, arg2) <- Parser.getTwoArguments args
            symbol1 <- Parser.getSymbolOrAlias arg1
            symbol2 <- Parser.getSymbolOrAlias arg2
            return Overload { getOverload = Just (symbol1, symbol2) }
        withApplication' = Parser.withApplication overloadId
        failDuplicate' = Parser.failDuplicate overloadId

    toAttributes =
        maybe def toAttribute . getOverload
      where
        toAttribute (symbol1, symbol2) =
            Attributes [overloadAttribute symbol1 symbol2]
