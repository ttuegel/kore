{- |
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
 -}
module Kore.Attribute.Attributes
    ( Attributes (..)
    , AttributePattern
    , asAttributePattern
    , attributePattern
    , attributePattern_
    , attributeString
    -- * Re-exports
    , PatternF (..)
    ) where

import           Control.DeepSeq
                 ( NFData )
import           Data.Default
                 ( Default (..) )
import           Data.Hashable
                 ( Hashable )
import           Data.Text
                 ( Text )
import qualified GHC.Generics as GHC

import Kore.Parser.Pattern
import Kore.Unparser

type AttributePattern = Pattern Variable

asAttributePattern :: PatternF Variable AttributePattern -> AttributePattern
asAttributePattern = asPattern

-- | An 'AttributePattern' of the attribute symbol applied to its arguments.
attributePattern
    :: SymbolOrAlias  -- ^ symbol
    -> [AttributePattern]  -- ^ arguments
    -> AttributePattern
attributePattern applicationSymbolOrAlias applicationChildren =
    (asAttributePattern . ApplicationF)
        Application { applicationSymbolOrAlias, applicationChildren }

-- | An 'AttributePattern' of the attribute symbol applied to no arguments.
attributePattern_
    :: SymbolOrAlias  -- ^ symbol
    -> AttributePattern
attributePattern_ applicationSymbolOrAlias =
    attributePattern applicationSymbolOrAlias []

-- | An 'AttributePattern' of the given literal string.
attributeString :: Text -> AttributePattern
attributeString text =
    (asAttributePattern . StringLiteralF) (StringLiteral text)

{-|'Attributes' corresponds to the @attributes@ Kore syntactic declaration.
It is parameterized by the types of Patterns, @pat@.
-}

newtype Attributes =
    Attributes { getAttributes :: [AttributePattern] }
    deriving (Eq, Ord, GHC.Generic, Show)

instance Hashable Attributes

instance NFData Attributes

instance Unparse Attributes where
    unparse = attributes . getAttributes
    unparse2 = attributes . getAttributes

instance Default Attributes where
    def = Attributes []
