{- |
Copyright : (c) 2020 Runtime Verification
License   : NCSA

 -}

module Prelude.Kore
    ( module Prelude
    , module Debug.Trace
    -- * Functions
    , (&)
    , on
    -- * Maybe
    , isJust
    , isNothing
    , fromMaybe
    , headMay
    -- * Either
    , either
    , isLeft, isRight
    -- * Filterable
    , Filterable (..)
    -- * Errors
    , HasCallStack
    , assert
    -- * Applicative and Alternative
    , Applicative (..)
    , Alternative (..)
    , optional
    -- * From
    , module From
    -- * Typeable
    , Typeable
    ) where

-- TODO (thomas.tuegel): Give an explicit export list so that the generated
-- documentation is complete.

import Control.Applicative
    ( Alternative (..)
    , Applicative (..)
    , optional
    )
import Control.Error
    ( either
    , headMay
    , isLeft
    , isRight
    )
import Control.Exception
    ( assert
    )
import Data.Function
    ( on
    , (&)
    )
import Data.Maybe
    ( fromMaybe
    , isJust
    , isNothing
    )
import Data.Typeable
    ( Typeable
    )
import Data.Witherable
    ( Filterable (..)
    )
import Debug.Trace
import From
import GHC.Stack
    ( HasCallStack
    )
import Prelude hiding
    ( Applicative (..)
    , either
    , filter
    , log
    )
