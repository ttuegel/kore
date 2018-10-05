{-|
Module      : Kore.Step.Simplifier
Description : Concrete monad for running the simplifier
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
-}
module Kore.Step.Simplifier
    ( Simplifier
    , runSimplifier
    , evalSimplifier
    ) where

import Kore.Variables.Fresh

type Simplifier = Counter

{- | Run a simplifier computation.

  The result is returned along with the final 'Counter'.

 -}
runSimplifier
    :: Simplifier a
    -- ^ simplifier computation
    -> Natural
    -- ^ initial counter for fresh variables
    -> (a, Natural)
runSimplifier = runCounter

{- | Evaluate a simplifier computation.

  Only the result is returned. The 'IntCounter' is discarded.

  -}
evalSimplifier :: Simplifier a -> a
evalSimplifier simplifier =
    let
        (result, _) = runSimplifier simplifier 0
    in
      result
