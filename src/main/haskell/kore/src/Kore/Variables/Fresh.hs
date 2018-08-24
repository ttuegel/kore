module Kore.Variables.Fresh
    ( FreshVariable (..)
    , MonadCounter
    ) where

import           Control.Monad.Counter
                 ( Counter (..), MonadCounter )
import qualified Control.Monad.Counter as Counter
import qualified Kore.AST.Common as Kore
import           Kore.AST.MetaOrObject
                 ( IsMetaOrObject (..), MetaOrObject (..) )

class FreshVariable v where
    freshVariable
        :: (MetaOrObject level, MonadCounter m) => v level -> m (v level)

    freshVariableSuchThat
        :: (MetaOrObject level, MonadCounter m)
        => v level
        -> (v level -> Bool)
        -> m (v level)
    freshVariableSuchThat var predicate =
        do
            var' <- freshVariable var
            if predicate var'
                then return var'
                else error "Cannot generate variable satisfying predicate"

instance FreshVariable Kore.Variable where
    freshVariable var =
        do
            n <- getCounter <$> Counter.increment
            return var
                { Kore.variableName = Kore.Id
                    { getId = metaObjectPrefix ++ "var_" ++ show n
                    , idLocation = Kore.AstLocationGeneratedVariable
                    }
                }
      where
        metaObjectPrefix =
            case isMetaOrObject var of
                IsObject -> ""
                IsMeta   -> "#"
