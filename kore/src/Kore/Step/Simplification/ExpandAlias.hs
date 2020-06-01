{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}

module Kore.Step.Simplification.ExpandAlias
    ( expandAlias
    , substituteInAlias
    ) where

import Prelude.Kore

import Control.Error
    ( MaybeT
    )
import Control.Error.Util
    ( nothing
    )
import qualified Data.Map.Strict as Map

import Kore.Internal.Alias
    ( Alias (..)
    )
import Kore.Internal.Pattern
    ( Pattern
    )
import Kore.Internal.TermLike
    ( pattern ApplyAlias_
    , InternalVariable
    , TermLike
    , Variable (..)
    , VariableName
    , fromVariableName
    , mapSomeVariableName
    , mapVariables
    , substitute
    )
import Kore.Unification.Unify
    ( MonadUnify
    )

expandAlias
    :: forall variable unifier
    .  InternalVariable variable
    => MonadUnify unifier
    => (   TermLike variable
        -> TermLike variable
        -> MaybeT unifier (Pattern variable)
       )
    -> TermLike variable
    -> TermLike variable
    -> MaybeT unifier (Pattern variable)
expandAlias recurse t1 t2 =
    case (expandSingleAlias t1, expandSingleAlias t2) of
        (Nothing, Nothing) -> nothing
        (t1', t2') -> recurse (fromMaybe t1 t1') (fromMaybe t2 t2')

expandSingleAlias
    :: InternalVariable variable
    => TermLike variable
    -> Maybe (TermLike variable)
expandSingleAlias =
    \case
        ApplyAlias_ alias children -> pure $ substituteInAlias alias children
        _ -> Nothing

-- TODO(Vladimir): Check aliases such that the intersection of alias variables
-- with the *bounded* variables in the rhs is empty (because we can't currently
-- handle things like \mu.
substituteInAlias
    :: InternalVariable variable
    => Alias (TermLike VariableName)
    -> [TermLike variable]
    -> TermLike variable
substituteInAlias Alias { aliasLeft, aliasRight } children =
    assert (length aliasLeft == length children)
    $ substitute substitutionMap
    $ mapVariables (pure fromVariableName) aliasRight
  where
    aliasLeft' =
        mapSomeVariableName (pure fromVariableName) . variableName
        <$> aliasLeft
    substitutionMap = Map.fromList $ zip aliasLeft' children
