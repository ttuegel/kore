{-|
Module      : Kore.ASTUtils.SmartConstructors
Description : Tree-based proof system, which can be
              hash-consed into a list-based one.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : phillip.harris@runtimeverification.com
Stability   : experimental
Portability : portable
-}

-- {-# OPTIONS_GHC -Wno-name-shadowing #-}

module Kore.ASTUtils.SmartConstructors
    ( -- * Utility functions for dealing with sorts
      getSort
    , forceSort
    , predicateSort
    , hasRigidHead
    , hasFlexibleHead
    , makeSortsAgree
    , ensureSortAgreement
    , isObviouslyPredicate
    -- * Lenses -- all applicative
    , patternLens
    , inputSort   -- | will have 0 or 1 inhabitants
    , resultSort  -- | will have 0 or 1 inhabitants
    , variable    -- | will have 0 or 1 inhabitants
    , allChildren -- | will have 0+ inhabitants
    , changeVar   -- | combinator for changing the `var` type in a pattern
    , inPath
    , localInPattern
    -- * Smart constructors
    , mkAnd
    , mkApp
    , mkBottom
    , mkCeil
    , mkDomainValue
    , mkEquals
    , mkExists
    , mkFloor
    , mkForall
    , mkIff
    , mkImplies
    , mkIn
    , mkNext
    , mkNot
    , mkOr
    , mkRewrites
    , mkTop
    , mkVar
    , mkStringLiteral
    , mkCharLiteral
    , mkSort
    , varS
    , symS
    ) where


import Control.Lens
import Control.Monad.State
import Data.Foldable
import Data.Functor.Foldable
import Data.Reflection

import Kore.AST.Common
import Kore.AST.MetaOrObject
import Kore.AST.MLPatterns
import Kore.ASTUtils.SmartPatterns
import Kore.IndexedModule.MetadataTools


-- | Gets the sort of of a pattern, taking the Metadatatools implicitly
-- from the context.
-- The smart constructors `mkAnd`, etc also require this context.
-- Usage: give metadatatools (... computation with Given Metadatatools ..)
getSort
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level), SortedVariable var)
    => PureMLPattern level var
    -> Sort level
getSort x = getPatternResultSort given $ project x

-- | Placeholder sort for when we construct a new predicate
-- But we don't know yet where it's going to be attached.
-- No particular way to avoid this, unfortunately.
-- This will probably happen often during proof routines.
predicateSort
    :: MetaOrObject level
    => Sort level
predicateSort = mkSort "PREDICATE"

patternLens
    :: (Applicative f, MetaOrObject level)
    => (Sort level -> f (Sort level))
    -> (Sort level -> f (Sort level))
    -> (var level -> f (var1 level))
    -> (PureMLPattern level var -> f (PureMLPattern level var1))
    -> (PureMLPattern level var -> f (PureMLPattern level var1))
patternLens
  i   -- input sort
  o   -- result sort
  var -- variable
  c   -- child
  = \case
  And_       s2   a b -> And_      <$>          o s2           <*> c a <*> c b
  Bottom_    s2       -> Bottom_   <$>          o s2
  Ceil_   s1 s2   a   -> Ceil_     <$> i s1 <*> o s2           <*> c a
  -- DV_        s2   a   -> DV_       <$>          o s2           <*> c a
  Fix (DomainValuePattern (DomainValue s2 a )) -> DV_ <$> o s2 <*> pure a
  Equals_ s1 s2   a b -> Equals_   <$> i s1 <*> o s2           <*> c a <*> c b
  Exists_    s2 v a   -> Exists_   <$>          o s2 <*> var v <*> c a
  Floor_  s1 s2   a   -> Floor_    <$> i s1 <*> o s2           <*> c a
  Forall_    s2 v a   -> Forall_   <$>          o s2 <*> var v <*> c a
  Iff_       s2   a b -> Iff_      <$>          o s2           <*> c a <*> c b
  Implies_   s2   a b -> Implies_  <$>          o s2           <*> c a <*> c b
  In_     s1 s2   a b -> In_       <$> i s1 <*> o s2           <*> c a <*> c b
  -- Next_      s2   a   -> Next_     <$>          o s2           <*> c a
  Fix (NextPattern (Next s2 a)) -> Next_ <$> o s2 <*> c a
  Not_       s2   a   -> Not_      <$>          o s2           <*> c a
  Or_        s2   a b -> Or_       <$>          o s2           <*> c a <*> c b
  -- Rewrites_  s2   a b -> Rewrites_ <$>          o s2           <*> c a <*> c b
  Fix (RewritesPattern (Rewrites s2 a b)) -> Rewrites_ <$> o s2 <*> c a <*> c b
  Top_       s2       -> Top_      <$>          o s2
  Var_          v     -> Var_      <$>                   var v
  App_ h ps -> App_ h <$> traverse c ps
  StringLiteral_ str  -> pure (StringLiteral_ str)
  CharLiteral_   char -> pure (CharLiteral_   char)
  _ -> error "The impossible happened."
  -- p -> pure p

-- | The sort of a,b in \equals(a,b), \ceil(a) etc.
inputSort
    :: MetaOrObject level
    => Traversal' (PureMLPattern level var) (Sort level)
inputSort        f = patternLens f    pure pure pure
-- | The sort returned by a top level constructor.
-- NOTE ABOUT NOTATION:
-- In the this haskell code, this is always `s2`.
-- In the semantics.pdf documentation, the sorts are written
-- {s1} if there is one sort parameter, and {s1, s2}
-- if there are two sort parameters. This has the effect
-- that the result sort is sometimes `s1` and sometimes `s2`.
-- I always refer to the result sort as `s2`, even if
-- there is no `s1`.
-- I believe this convention is less confusing.
-- Note that a few constructors like App and StringLiteral
-- lack a result sort in the AST.
resultSort
    :: MetaOrObject level
    => Traversal' (PureMLPattern level var) (Sort level)
resultSort       f = patternLens pure f    pure pure
-- | Points to the bound variable in Forall/Exists,
-- and also the Variable in VariablePattern
variable
    :: MetaOrObject level
    => Traversal' (PureMLPattern level var) (var level)
variable         f = patternLens pure pure f    pure
-- All sub-expressions which are Patterns.
-- use partsOf allChildren to get a lens to a List.
allChildren
    :: MetaOrObject level
    => Traversal' (PureMLPattern level var) (PureMLPattern level var)
allChildren      f = patternLens pure pure pure f

changeVar
    :: (MetaOrObject level, Applicative f)
    => (var level -> f (var1 level))
    -> (PureMLPattern level var -> f (PureMLPattern level var1))
    -> (PureMLPattern level var -> f (PureMLPattern level var1))
changeVar v f = patternLens pure pure v f

-- | Applies a function at an `[Int]` path.
localInPattern
    :: MetaOrObject level
    => [Int]
    -> (PureMLPattern level var -> PureMLPattern level var)
    -> PureMLPattern level var
    -> PureMLPattern level var
localInPattern path f pat = pat & inPath path %~ f


-- | Takes an `[Int]` representing a path, and returns a lens to that position.
-- The ints represent subpatterns in the obvious way:
-- [0,1] points to b in \ceil(a /\ b), etc.
inPath
    :: (MetaOrObject level, Applicative f)
    => [Int]
    -> (PureMLPattern level var -> f (PureMLPattern level var))
    -> (PureMLPattern level var -> f (PureMLPattern level var))
inPath []       = id --aka the identity lens
inPath (n : ns) = partsOf allChildren . ix n . inPath ns

-- | Rigid pattern heads are those which have a
-- single uniquely determined sort,
-- which we can't change.
hasRigidHead
    :: MetaOrObject level
    => PureMLPattern level var
    -> Bool
hasRigidHead p = case project p of
    ApplicationPattern   _ -> True
    DomainValuePattern   _ -> True
    VariablePattern      _ -> True
    StringLiteralPattern _ -> True
    CharLiteralPattern   _ -> True
    _                      -> False


-- | Flexible pattern heads are those which can be
-- any sort, like predicates \equals, \ceil etc.
-- The 3rd possibility (not hasFlexibleHead && not hasRigidHead)
-- is a constructor whose sort
-- must match the sort of of its subexpressions:
-- \and, \or, \implies, etc.
hasFlexibleHead
    :: MetaOrObject level
    => PureMLPattern level var
    -> Bool
hasFlexibleHead p = case project p of
    BottomPattern _ -> True
    CeilPattern   _ -> True
    EqualsPattern _ -> True
    FloorPattern  _ -> True
    InPattern     _ -> True
    TopPattern    _ -> True
    _               -> False

-- | Attempts to modify p to have sort s.
forceSort
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level), SortedVariable var)
    => Sort level
    -> PureMLPattern level var
    -> Maybe (PureMLPattern level var)
forceSort s p
  | getSort p == s = Just p
  | hasRigidHead    p   = Nothing
  | hasFlexibleHead p   = Just $ p & resultSort .~ s
  | otherwise      = traverseOf allChildren (forceSort s) p

-- | Modify all patterns in a list to have the same sort.
makeSortsAgree
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level), SortedVariable var)
    => [PureMLPattern level var]
    -> Maybe [PureMLPattern level var]
makeSortsAgree ps =
    forM ps $ forceSort $
        case asum $ getRigidSort <$> ps of
          Nothing -> predicateSort
          Just a  -> a

getRigidSort
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level), SortedVariable var)
    => PureMLPattern level var
    -> Maybe (Sort level)
getRigidSort p =
    case forceSort predicateSort p of
      Nothing -> Just $ getSort p
      Just _  -> Nothing

-- | Ensures that the subpatterns of a pattern match in their sorts
-- and assigns the correct sort to the top level pattern
-- i.e. converts the invalid (x : Int /\ ( x < 3 : Float)) : Bool
-- to the valid (x : Int /\ (x < 3 : Int)) : Int
ensureSortAgreement
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
ensureSortAgreement p =
  case makeSortsAgree $ p ^. partsOf allChildren of
    Just []    -> p & resultSort .~ predicateSort
    Just ps@(c : _) ->
      p & (partsOf allChildren) .~ ps
        & inputSort  .~ getSort c
        & resultSort .~ (
          if hasFlexibleHead p
            then predicateSort
            else getSort c
          )
    Nothing -> error $ "Can't unify sorts of subpatterns: " ++ show p

-- | In practice, all the predicate patterns we use are
-- composed of =, \floor, \ceil, and \in. I haven't come
-- across a single counterexample. Thus this function can
-- probably be trusted to tell you if something is a
-- predicate. Note that `isObviouslyPredicate` and
-- `hasFlexibleHead` are NOT the same. `hasFlexibleHead` only
-- looks at the head of the pattern, it will return false
-- for `a = b /\ c = d`, whereas `isObviouslyPredicate` will
-- traverse the whole pattern and return True.
-- Also, in practice, having a flexible sort and being a predicate
-- are synonymous. But don't quote me on this.
isObviouslyPredicate
    :: PureMLPattern level var
    -> Bool
isObviouslyPredicate = \case
  And_ _       a b -> isObviouslyPredicate a && isObviouslyPredicate b
  Or_  _       a b -> isObviouslyPredicate a && isObviouslyPredicate b
  Implies_ _   a b -> isObviouslyPredicate a && isObviouslyPredicate b
  Iff_ _       a b -> isObviouslyPredicate a && isObviouslyPredicate b
  Not_ _       a   -> isObviouslyPredicate a
  Forall_ _ _  a   -> isObviouslyPredicate a
  Exists_ _ _  a   -> isObviouslyPredicate a
  Equals_ _ _ _ _  -> True
  Ceil_ _ _ _      -> True
  Floor_ _ _ _     -> True
  In_ _ _ _ _      -> True
  _ -> False

-- | Constructors that handle sort information automatically.
-- To use, put `give metadatatools` at the top of the computation.
mkAnd
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkAnd a b = ensureSortAgreement $ And_ fixmeSort a b

-- TODO: Should this check for sort agreement?
mkApp
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level))
    => SymbolOrAlias level
    -> [PureMLPattern level var]
    -> PureMLPattern level var
mkApp = App_

mkBottom
    :: MetaOrObject level
    => PureMLPattern level var
mkBottom = Bottom_ predicateSort

mkCeil
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level), SortedVariable var)
    => PureMLPattern level var
    -> PureMLPattern level var
mkCeil a = Ceil_ (getSort a) predicateSort a

mkDomainValue
    :: (MetaOrObject Object, Given (SymbolOrAliasSorts Object))
    => Sort Object
    -> BuiltinDomain (CommonPurePattern Meta)
    -> PureMLPattern Object var
mkDomainValue = DV_

mkEquals
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkEquals a b = ensureSortAgreement $ Equals_ fixmeSort fixmeSort a b

mkExists
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => var level
    -> PureMLPattern level var
    -> PureMLPattern level var
mkExists v a = ensureSortAgreement $ Exists_ fixmeSort v a

mkFloor
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
mkFloor a = ensureSortAgreement $ Floor_ fixmeSort fixmeSort a

mkForall
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => var level
    -> PureMLPattern level var
    -> PureMLPattern level var
mkForall v a = ensureSortAgreement $ Forall_ fixmeSort v a

mkIff
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkIff a b = ensureSortAgreement $ Iff_ fixmeSort a b

mkImplies
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkImplies a b = ensureSortAgreement $ Implies_ fixmeSort a b

mkIn
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkIn a b = ensureSortAgreement $ In_ fixmeSort fixmeSort a b

mkNext
    ::  ( MetaOrObject Object
        , Given (SymbolOrAliasSorts Object)
        , SortedVariable var
        , Show (var Object))
    => PureMLPattern Object var
    -> PureMLPattern Object var
mkNext a = ensureSortAgreement $ Next_ fixmeSort a

mkNot
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
mkNot a = ensureSortAgreement $ Not_ fixmeSort a

mkOr
    ::  ( MetaOrObject level
        , Given (SymbolOrAliasSorts level)
        , SortedVariable var
        , Show (var level))
    => PureMLPattern level var
    -> PureMLPattern level var
    -> PureMLPattern level var
mkOr a b = ensureSortAgreement $ Or_ fixmeSort a b

mkRewrites
    ::  ( MetaOrObject Object
        , Given (SymbolOrAliasSorts Object)
        , SortedVariable var
        , Show (var Object))
    => PureMLPattern Object var
    -> PureMLPattern Object var
    -> PureMLPattern Object var
mkRewrites a b = ensureSortAgreement $ Rewrites_ fixmeSort a b

mkTop
    :: MetaOrObject level
    => PureMLPattern level var
mkTop = Top_ predicateSort

mkVar
    :: (MetaOrObject level, Given (SymbolOrAliasSorts level))
    => var level
    -> PureMLPattern level var
mkVar = Var_

mkStringLiteral :: String -> PureMLPattern Meta var
mkStringLiteral = StringLiteral_

mkCharLiteral :: Char -> PureMLPattern Meta var
mkCharLiteral = CharLiteral_

mkSort
  :: MetaOrObject level
  => String
  -> Sort level
mkSort name =
    SortActualSort $ SortActual (noLocationId name) []

-- | Construct a variable with a given name and sort
-- "x" `varS` s
varS :: MetaOrObject level => String -> Sort level -> Variable level
varS x s =
    Variable (noLocationId x) s

-- | Construct a symbol with a given name and input sorts
-- "mult" `symS` [s, s]
-- Since the return sort is only found in MetadataTools, this is
-- mostly useful for testing.
symS :: MetaOrObject level => String -> [Sort level] -> SymbolOrAlias level
symS x s =
    SymbolOrAlias (noLocationId x) s

-- | Placeholder. Should never appear in output of 'mk' funcs
fixmeSort
    :: MetaOrObject level
    => Sort level
fixmeSort = mkSort "FIXME"
