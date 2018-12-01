{-|
Module      : Kore.ASTUtils.Substitution
Description : Substitute phi_1 for phi_2, avoiding capture
              In particular this implements axiom 7 in
              the "large" axiom set (Rosu 2017).
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Kore.ASTUtils.Substitution
( subst
, localSubst
, freeVars
)
where

import           Control.Lens
import           Data.Functor.Classes
                 ( Eq1 )
import           Data.Functor.Foldable
import qualified Data.Set as S
import qualified Data.Text as Text

import Kore.Annotation.Valid
import Kore.AST.Pure
import Kore.ASTUtils.SmartConstructors
import Kore.ASTUtils.SmartPatterns

-- | subst phi_1 phi_2 phi = phi[phi_2/phi_1]
-- Note that different papers use different conventions.
-- Here `phi_1` is the old pattern that disappears
-- and `phi_2` is the new pattern that replaces it.
subst
    ::  ( Traversable dom
        , MetaOrObject level
        , Eq1 dom
        )
    => CommonPurePattern level dom (Valid level)
    -> CommonPurePattern level dom (Valid level)
    -> CommonPurePattern level dom (Valid level)
    -> CommonPurePattern level dom (Valid level)
subst old new = \case
    Forall_ s1 v p -> handleBinder old new mkForall v p
    Exists_ s1 v p -> handleBinder old new mkExists v p
    pat
     | pat == old -> new
     | otherwise  -> embed $ fmap (subst old new) $ project pat

handleBinder old new binder v p
  | S.member v (freeVars old) = binder v p
  | S.member v (freeVars new) = subst old new $ alphaRename binder v p
  | otherwise = binder v $ subst old new p
  where
    alphaRename binder v p =
        binder (replacementVar v p)
        (subst (mkCommonVar v) (mkCommonVar $ replacementVar v p) p)
    replacementVar v p =
        head $ filter (not . flip S.member freeVarsP) $ alternatives v
    freeVarsP = freeVars p
    alternatives (Variable (Id name loc) sort) =
        [Variable (Id (name <> (Text.pack . show) n) loc) sort | n <- [(0::Integer)..] ]

freeVars
    :: (MetaOrObject level, Traversable dom)
    => CommonPurePattern level dom ann
    -> S.Set (Variable level)
freeVars = \case
    Forall_ s1 v p -> S.delete v $ freeVars p
    Exists_ s1 v p -> S.delete v $ freeVars p
    Var_ v -> S.singleton v
    p -> S.unions $ map freeVars $ p ^. partsOf allChildren

-- | Apply a substitution at every eligible position below the specified path.
-- This is technically less general than axiom 7, which allows for
-- substituting at an arbitrary set of eligible positions,
-- but it doesn't matter in practice.
localSubst
    :: (Eq1 dom, MetaOrObject level, Traversable dom)
    => CommonPurePattern level dom (Valid level)
    -> CommonPurePattern level dom (Valid level)
    -> [Int]
    -> CommonPurePattern level dom (Valid level)
    -> CommonPurePattern level dom (Valid level)
localSubst a b path pat = localInPattern path (subst a b) pat
