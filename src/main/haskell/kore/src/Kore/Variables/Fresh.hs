{-|
Module      : Kore.Variables.Fresh
Description : Specify an interface for generating fresh variables
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

-}
module Kore.Variables.Fresh
    ( FreshVariable (..)
    , MonadCounter (..)
    , Counter (..)
    , Counting, runCounting, evalCounting
    ) where

import Control.Monad.Counter
import Kore.AST.Common
       ( AstLocation (..), Id (..), Variable (..) )
import Kore.AST.MetaOrObject

{- | A 'FreshVariable' can be freshened in a 'MonadCounter'.
-}
class FreshVariable var where
    {-|Given an existing variable, generate a fresh one of
    the same kind.
    -}
    freshVariable :: (MetaOrObject level, MonadCounter m) => var level -> m (var level)

    {-|Given an existing variable and a predicate, generate a
    fresh variable of the same kind satisfying the predicate.
    By default, die in flames if the predicate is not satisfied.
    -}
    freshVariableSuchThat
        :: (MetaOrObject level, MonadCounter m)
        => var level
        -> (var level -> Bool)
        -> m (var level)
    freshVariableSuchThat var p = do
        var' <- freshVariable var
        if p var'
            then return var'
            else error "Cannot generate variable satisfying predicate"


instance FreshVariable Variable where
    freshVariable var = do
        Counter n <- increment
        pure
            (var
                { variableName = Id
                    { getId = metaObjectPrefix ++ "var_" ++ show n
                    , idLocation = AstLocationGeneratedVariable
                    }
                }
            )
      where
        metaObjectPrefix =
            case isMetaOrObject var of
                IsObject -> ""
                IsMeta   -> "#"
