{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
{-|
Module      : Data.Kore.Unparser.Unparse
Description : Class for unparsing and instances for it for 'Meta' and
              unified Kore constructs.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Data.Kore.Unparser.Unparse (Unparse(..), unparseToString) where

import           Data.Text.Prettyprint.Doc.Render.String (renderString)

import           Data.Kore.AST.Pretty

-- |'Unparse' class offers functionality to reverse the parsing process.
class Pretty a => Unparse a where
    unparse :: a -> Doc ann
    unparse = pretty

instance Pretty a => Unparse a

-- |'unparseToString' uses a 'StringPrinter' to serialize an object to 'String'.
unparseToString :: Unparse a => a -> String
unparseToString = renderString . layoutPrettyUnbounded . pretty
