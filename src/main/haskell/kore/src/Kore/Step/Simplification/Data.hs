{-|
Module      : Kore.Step.Simplification.Data
Description : Data structures used for term simplification.
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Data
    ( SimplificationError
    , Simplifier
    , runSimplifier, evalSimplifier
    , PureMLPatternSimplifier (..)
    , CommonPureMLPatternSimplifier
    , SimplificationProof (..)
    ) where

import Control.Monad.Except
       ( ExceptT, runExceptT )

import Kore.AST.Common
       ( Variable )
import Kore.AST.PureML
       ( PureMLPattern )
import Kore.Error
       ( Error )
import Kore.Step.OrOfExpandedPattern
       ( OrOfExpandedPattern )
import Kore.Variables.Fresh


{- | A tag for errors during simplification

  See also: 'Error'

 -}
data SimplificationError

{-| 'SimplificationProof' is a placeholder for proofs showing that the
simplification of a MetaMLPattern was correct.
-}
data SimplificationProof level = SimplificationProof
    deriving (Show, Eq)

{- | The concrete monad in which simplification occurs.

 -}
-- TODO (thomas.tuegel): Lift the StateT to the outer level.
type Simplifier = ExceptT (Error SimplificationError) Counting

{- | Run a simplifier computation.

  The result (or error) is returned along with the final 'Counter'.

 -}
runSimplifier :: Simplifier a -> Counter -> (Either (Error SimplificationError) a, Counter)
runSimplifier simplifier = runCounting (runExceptT simplifier)

{- | Evaluate a simplifier computation.

  Only the result (or error) is returned. The 'Counter' is discarded.

  -}
evalSimplifier :: Simplifier a -> Either (Error SimplificationError) a
evalSimplifier simplifier =
    let
        (result, _) = runSimplifier simplifier (Counter 0)
    in
      result

{-| 'PureMLPatternSimplifier' wraps a function that evaluates
Kore functions on PureMLPatterns.
-}
newtype PureMLPatternSimplifier level variable =
    PureMLPatternSimplifier
        ( PureMLPattern level variable
        -> Simplifier
            ( OrOfExpandedPattern level variable
            , SimplificationProof level
            )
        )

{-| 'CommonPurePatternFunctionEvaluator' wraps a function that evaluates
Kore functions on CommonPurePatterns.
-}
type CommonPureMLPatternSimplifier level =
    PureMLPatternSimplifier level Variable
