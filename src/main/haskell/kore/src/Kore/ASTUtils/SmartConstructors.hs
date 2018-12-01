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
    , mkVar'
    , mkCommonVar
    , mkStringLiteral
    , mkCharLiteral
    , mkSort
    , varS
    , symS
    ) where

import           Control.Comonad
import           Control.Lens hiding
                 ( (:<) )
import           Control.Monad.State
import           Data.Foldable
import qualified Data.Functor.Foldable as Recursive
import           Data.Reflection
import           Data.Text
                 ( Text )
import           GHC.Stack
                 ( HasCallStack )

import Kore.Annotation.Valid
import Kore.AST.Pure
import Kore.Unparser

-- | Gets the sort of of a pattern, taking the Metadatatools implicitly
-- from the context.
-- The smart constructors `mkAnd`, etc also require this context.
-- Usage: give metadatatools (... computation with Given Metadatatools ..)
getSort
    ::  ( MetaOrObject level
        , Functor dom
        )
    => PurePattern level dom var (Valid level)
    -> Sort level
getSort purePattern =
    patternSort
  where
    Valid { patternSort } = extract purePattern

-- | Placeholder sort for when we construct a new predicate
-- But we don't know yet where it's going to be attached.
-- No particular way to avoid this, unfortunately.
-- This will probably happen often during proof routines.
predicateSort
    :: MetaOrObject level
    => Sort level
predicateSort = mkSort "PREDICATE"

patternLens
    :: forall f lvl dom var var1 ann.
        ( Applicative f, MetaOrObject lvl, Traversable dom)
    => (Sort lvl -> f (Sort lvl))
    -> (Sort lvl -> f (Sort lvl))
    -> (var lvl -> f (var1 lvl))
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var1 ann))
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var1 ann))
patternLens
    lensOperandSort   -- input sort
    lensResultSort   -- result sort
    lensVariable -- variable
    lensChild   -- child
  = \(Recursive.project -> ann :< pat) ->
    Recursive.embed . (ann :<) <$> patternLensWorker pat
  where
    patternLensWorker =
        \case
            AndPattern and0 -> AndPattern <$> patternLensAnd and0
            BottomPattern bot0 -> BottomPattern <$> patternLensBottom bot0
            CeilPattern ceil0 -> CeilPattern <$> patternLensCeil ceil0
            DomainValuePattern dv0 ->
                DomainValuePattern <$> patternLensDomainValue dv0
            EqualsPattern eq0 -> EqualsPattern <$> patternLensEquals eq0
            ExistsPattern ex0 -> ExistsPattern <$> patternLensExists ex0
            FloorPattern flr0 -> FloorPattern <$> patternLensFloor flr0
            ForallPattern fa0 -> ForallPattern <$> patternLensForall fa0
            IffPattern iff0 -> IffPattern <$> patternLensIff iff0
            ImpliesPattern imp0 -> ImpliesPattern <$> patternLensImplies imp0
            InPattern in0 -> InPattern <$> patternLensIn in0
            NextPattern next0 -> NextPattern <$> patternLensNext next0
            NotPattern not0 -> NotPattern <$> patternLensNot not0
            OrPattern or0 -> OrPattern <$> patternLensOr or0
            RewritesPattern rew0 -> RewritesPattern <$> patternLensRewrites rew0
            TopPattern top0 -> TopPattern <$> patternLensTop top0
            VariablePattern var0 -> VariablePattern <$> lensVariable var0
            ApplicationPattern app0 ->
                ApplicationPattern <$> patternLensApplication app0
            StringLiteralPattern lit -> pure (StringLiteralPattern lit)
            CharLiteralPattern lit -> pure (CharLiteralPattern lit)

    patternLensAnd And { andSort, andFirst, andSecond } =
        And
            <$> lensResultSort andSort
            <*> lensChild andFirst
            <*> lensChild andSecond

    patternLensBottom Bottom { bottomSort } =
        Bottom <$> lensResultSort bottomSort

    patternLensCeil Ceil { ceilOperandSort, ceilResultSort, ceilChild } =
        Ceil
            <$> lensOperandSort ceilOperandSort
            <*> lensResultSort ceilResultSort
            <*> lensChild ceilChild

    patternLensDomainValue
        :: lvl ~ Object
        => DomainValue lvl dom (PurePattern lvl dom var ann)
        -> f (DomainValue lvl dom (PurePattern lvl dom var1 ann))
    patternLensDomainValue DomainValue { domainValueSort, domainValueChild } =
        DomainValue
            <$> lensResultSort domainValueSort
            <*> traverse lensChild domainValueChild

    patternLensEquals
        Equals
            { equalsOperandSort
            , equalsResultSort
            , equalsFirst
            , equalsSecond
            }
      =
        Equals
            <$> lensOperandSort equalsOperandSort
            <*> lensResultSort equalsResultSort
            <*> lensChild equalsFirst
            <*> lensChild equalsSecond

    patternLensExists Exists { existsSort, existsVariable, existsChild } =
        Exists
            <$> lensResultSort existsSort
            <*> lensVariable existsVariable
            <*> lensChild existsChild

    patternLensFloor Floor { floorOperandSort, floorResultSort, floorChild } =
        Floor
            <$> lensOperandSort floorOperandSort
            <*> lensResultSort floorResultSort
            <*> lensChild floorChild

    patternLensForall Forall { forallSort, forallVariable, forallChild } =
        Forall
            <$> lensResultSort forallSort
            <*> lensVariable forallVariable
            <*> lensChild forallChild

    patternLensIff Iff { iffSort, iffFirst, iffSecond } =
        Iff
            <$> lensResultSort iffSort
            <*> lensChild iffFirst
            <*> lensChild iffSecond

    patternLensImplies Implies { impliesSort, impliesFirst, impliesSecond } =
        Implies
            <$> lensResultSort impliesSort
            <*> lensChild impliesFirst
            <*> lensChild impliesSecond

    patternLensIn
        In
            { inOperandSort
            , inResultSort
            , inContainedChild
            , inContainingChild
            }
      =
        In
            <$> lensOperandSort inOperandSort
            <*> lensResultSort inResultSort
            <*> lensChild inContainedChild
            <*> lensChild inContainingChild

    patternLensNext Next { nextSort, nextChild } =
        Next
            <$> lensResultSort nextSort
            <*> lensChild nextChild

    patternLensNot Not { notSort, notChild } =
        Not
            <$> lensResultSort notSort
            <*> lensChild notChild

    patternLensOr Or { orSort, orFirst, orSecond } =
        Or
            <$> lensResultSort orSort
            <*> lensChild orFirst
            <*> lensChild orSecond

    patternLensRewrites
        Rewrites
            { rewritesSort
            , rewritesFirst
            , rewritesSecond
            }
      =
        Rewrites
            <$> lensResultSort rewritesSort
            <*> lensChild rewritesFirst
            <*> lensChild rewritesSecond

    patternLensTop Top { topSort } =
        Top <$> lensResultSort topSort

    patternLensApplication
        Application
            { applicationSymbolOrAlias
            , applicationChildren
            }
      =
        Application applicationSymbolOrAlias
            <$> traverse lensChild applicationChildren

-- | The sort of a,b in \equals(a,b), \ceil(a) etc.
inputSort
    :: (MetaOrObject lvl, Traversable dom)
    => Traversal' (PurePattern lvl dom var ann) (Sort lvl)
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
    :: (MetaOrObject lvl, Traversable dom)
    => Traversal' (PurePattern lvl dom var ann) (Sort lvl)
resultSort = \f -> patternLens pure f pure pure
-- | Points to the bound variable in Forall/Exists,
-- and also the Variable in VariablePattern
variable
    :: (MetaOrObject lvl, Traversable dom)
    => Traversal' (PurePattern lvl dom var ann) (var lvl)
variable = \f -> patternLens pure pure f pure
-- All sub-expressions which are Patterns.
-- use partsOf allChildren to get a lens to a List.
allChildren
    :: (MetaOrObject lvl, Traversable dom)
    => Traversal' (PurePattern lvl dom var ann) (PurePattern lvl dom var ann)
allChildren = patternLens pure pure pure

changeVar
    :: (MetaOrObject lvl, Applicative f, Traversable dom)
    => (var lvl -> f (var1 lvl))
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var1 ann))
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var1 ann))
changeVar = patternLens pure pure

-- | Applies a function at an `[Int]` path.
localInPattern
    :: (MetaOrObject lvl, Traversable dom)
    => [Int]
    -> (PurePattern lvl dom var ann -> PurePattern lvl dom var ann)
    -> PurePattern lvl dom var ann
    -> PurePattern lvl dom var ann
localInPattern path f pat = pat & inPath path %~ f

-- | Takes an `[Int]` representing a path, and returns a lens to that position.
-- The ints represent subpatterns in the obvious way:
-- [0,1] points to b in \ceil(a /\ b), etc.
inPath
    :: (MetaOrObject lvl, Applicative f, Traversable dom)
    => [Int]
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var ann))
    -> (PurePattern lvl dom var ann -> f (PurePattern lvl dom var ann))
inPath []       = id --aka the identity lens
inPath (n : ns) = partsOf allChildren . ix n . inPath ns

-- | Rigid pattern heads are those which have a
-- single uniquely determined sort,
-- which we can't change.
hasRigidHead
    :: (MetaOrObject lvl, Functor dom)
    => PurePattern lvl dom var ann
    -> Bool
hasRigidHead (Recursive.project -> _ :< pat) =
    case pat of
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
    :: (MetaOrObject lvl, Functor dom)
    => PurePattern lvl dom var ann
    -> Bool
hasFlexibleHead (Recursive.project -> _ :< pat) =
    case pat of
        BottomPattern _ -> True
        CeilPattern   _ -> True
        EqualsPattern _ -> True
        FloorPattern  _ -> True
        InPattern     _ -> True
        TopPattern    _ -> True
        _               -> False

-- | Attempts to modify p to have sort s.
forceSort
    ::  ( MetaOrObject lvl
        , Traversable dom
        )
    => Sort lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> Maybe (PurePattern lvl dom var (Valid lvl))
forceSort s p
  | getSort p == s = Just p
  | hasRigidHead    p   = Nothing
  | hasFlexibleHead p   = Just $ p & resultSort .~ s
  | otherwise      = traverseOf allChildren (forceSort s) p

-- | Modify all patterns in a list to have the same sort.
makeSortsAgree
    ::  ( MetaOrObject lvl
        , Traversable dom
        )
    => [PurePattern lvl dom var (Valid lvl)]
    -> Maybe [PurePattern lvl dom var (Valid lvl)]
makeSortsAgree ps =
    forM ps $ forceSort $
        case asum $ getRigidSort <$> ps of
          Nothing -> predicateSort
          Just a  -> a

getRigidSort
    ::  ( MetaOrObject lvl
        , Traversable dom
        )
    => PurePattern lvl dom var (Valid lvl)
    -> Maybe (Sort lvl)
getRigidSort p =
    case forceSort predicateSort p of
      Nothing -> Just $ getSort p
      Just _  -> Nothing

-- | Ensures that the subpatterns of a pattern match in their sorts
-- and assigns the correct sort to the top level pattern
-- i.e. converts the invalid (x : Int /\ ( x < 3 : Float)) : Bool
-- to the valid (x : Int /\ (x < 3 : Int)) : Int
ensureSortAgreement
    ::  ( MetaOrObject lvl
        , Unparse (PurePattern lvl dom var (Valid lvl))
        , Traversable dom
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
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
    Nothing ->
        (error . unlines)
            [ "Can't unify sorts of subpatterns:"
            , unparseToString p
            ]

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
    :: Functor dom
    => PurePattern lvl dom var ann
    -> Bool
isObviouslyPredicate (Recursive.project -> _ :< pat) =
    case pat of
        -- Trivial cases
        EqualsPattern _ -> True
        InPattern _ -> True
        CeilPattern _ -> True
        FloorPattern _ -> True
        -- Non-trivial cases
        AndPattern and0 -> all isObviouslyPredicate and0
        OrPattern or0 -> all isObviouslyPredicate or0
        ImpliesPattern imp0 -> all isObviouslyPredicate imp0
        IffPattern iff0 -> all isObviouslyPredicate iff0
        NotPattern not0 -> all isObviouslyPredicate not0
        ForallPattern all0 -> all isObviouslyPredicate all0
        ExistsPattern any0 -> all isObviouslyPredicate any0
        -- Non-predicates
        _ -> False

{- | Return the unified sort of two patterns.

An error is thrown if the patterns have different sorts.
 -}
unifySorts
    ::  ( Functor dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> Sort lvl
unifySorts pat1 pat2
  | sort1 == sort2 = sort1
  | otherwise =
    (error . unlines)
        [ "Cannot unify sort:"
        , unparseToString sort1
        , "with sort:"
        , unparseToString sort2
        , "in patterns:"
        , unparseToString pat1
        , unparseToString pat2
        ]
  where
    Valid { patternSort = sort1 } = extract pat1
    Valid { patternSort = sort2 } = extract pat2

-- | Constructors that handle sort information automatically.
-- To use, put `give metadatatools` at the top of the computation.
mkAnd
    ::  ( MetaOrObject lvl
        , HasCallStack
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkAnd andFirst andSecond =
    asPurePattern (valid :< AndPattern and0)
  where
    patternSort = unifySorts andFirst andSecond
    valid = Valid { patternSort }
    and0 = And { andSort = patternSort, andFirst, andSecond }

-- TODO: Should this check for sort agreement?
mkApp
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl
    -> SymbolOrAlias lvl
    -> [PurePattern lvl dom var (Valid lvl)]
    -> PurePattern lvl dom var (Valid lvl)
mkApp patternSort applicationSymbolOrAlias applicationChildren =
    asPurePattern (valid :< ApplicationPattern application)
  where
    valid = Valid { patternSort }
    application =
        Application { applicationSymbolOrAlias, applicationChildren }

mkBottom
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl -> PurePattern lvl dom var (Valid lvl)
mkBottom patternSort =
    asPurePattern (Valid { patternSort } :< BottomPattern bottom)
  where
    bottom = Bottom { bottomSort = patternSort }

mkCeil
    ::  ( MetaOrObject lvl
        , Functor dom
        )
    => Sort lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkCeil ceilResultSort ceilChild =
    asPurePattern (valid :< CeilPattern ceil)
  where
    valid = Valid { patternSort = ceilResultSort }
    Valid { patternSort = ceilOperandSort } = extract ceilChild
    ceil = Ceil { ceilOperandSort, ceilResultSort, ceilChild }

mkDomainValue
    :: (Functor dom, MetaOrObject Object)
    => Sort Object
    -> dom (PurePattern Object dom var (Valid Object))
    -> PurePattern Object dom var (Valid Object)
mkDomainValue domainValueSort domainValueChild =
    asPurePattern (valid :< DomainValuePattern domainValue)
  where
    domainValue = DomainValue { domainValueSort, domainValueChild }
    valid = Valid { patternSort = domainValueSort }

mkEquals
    ::  ( HasCallStack
        , MetaOrObject lvl
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => Sort lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkEquals equalsResultSort equalsFirst equalsSecond =
    asPurePattern (valid :< EqualsPattern equals)
  where
    valid = Valid { patternSort }
    patternSort = unifySorts equalsFirst equalsSecond
    equals =
        Equals
            { equalsOperandSort = patternSort
            , equalsResultSort
            , equalsFirst
            , equalsSecond
            }

mkExists
    :: (MetaOrObject lvl, Traversable dom)
    => var lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkExists existsVariable existsChild =
    asPurePattern (valid :< ExistsPattern exists)
  where
    Valid { patternSort = existsSort } = extract existsChild
    valid = Valid { patternSort = existsSort }
    exists = Exists { existsSort, existsVariable, existsChild }

mkFloor
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkFloor floorResultSort floorChild =
    asPurePattern (valid :< FloorPattern floor')
  where
    valid = Valid { patternSort = floorResultSort }
    Valid { patternSort = floorOperandSort } = extract floorChild
    floor' = Floor { floorOperandSort, floorResultSort, floorChild }

mkForall
    :: (MetaOrObject lvl, Traversable dom)
    => var lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkForall forallVariable forallChild =
    asPurePattern (valid :< ForallPattern forall)
  where
    Valid { patternSort = forallSort } = extract forallChild
    valid = Valid { patternSort = forallSort }
    forall = Forall { forallSort, forallVariable, forallChild }

mkIff
    ::  ( HasCallStack
        , MetaOrObject lvl
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkIff iffFirst iffSecond =
    asPurePattern (valid :< IffPattern iff0)
  where
    patternSort = unifySorts iffFirst iffSecond
    valid = Valid { patternSort }
    iff0 = Iff { iffSort = patternSort, iffFirst, iffSecond }

mkImplies
    ::  ( HasCallStack
        , MetaOrObject lvl
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkImplies impliesFirst impliesSecond =
    asPurePattern (valid :< ImpliesPattern implies0)
  where
    patternSort = unifySorts impliesFirst impliesSecond
    valid = Valid { patternSort }
    implies0 =
        Implies
            { impliesSort = patternSort
            , impliesFirst
            , impliesSecond
            }

mkIn
    ::  ( HasCallStack
        , MetaOrObject lvl
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => Sort lvl
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkIn inResultSort inContainedChild inContainingChild =
    asPurePattern (valid :< InPattern in0)
  where
    valid = Valid { patternSort }
    patternSort = unifySorts inContainedChild inContainingChild
    in0 =
        In
            { inOperandSort = patternSort
            , inResultSort
            , inContainedChild
            , inContainingChild
            }

mkNext
    ::  ( MetaOrObject Object
        , Unparse (PurePattern Object dom var (Valid Object))
        , Traversable dom
        )
    => PurePattern Object dom var (Valid Object)
    -> PurePattern Object dom var (Valid Object)
mkNext nextChild =
    ensureSortAgreement $ asPurePattern (valid :< NextPattern next)
  where
    valid@Valid { patternSort } = extract nextChild
    next = Next { nextSort = patternSort, nextChild }

mkNot
    :: (MetaOrObject lvl, Traversable dom)
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkNot notChild =
    asPurePattern (valid :< NotPattern not0)
  where
    valid@Valid { patternSort } = extract notChild
    not0 = Not { notSort = patternSort, notChild }

mkOr
    ::  ( HasCallStack
        , MetaOrObject lvl
        , Traversable dom
        , Unparse (PurePattern lvl dom var (Valid lvl))
        )
    => PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkOr orFirst orSecond =
    asPurePattern (valid :< OrPattern or0)
  where
    patternSort = unifySorts orFirst orSecond
    valid = Valid { patternSort }
    or0 = Or { orSort = patternSort, orFirst, orSecond }

mkRewrites
    ::  ( MetaOrObject Object
        , Unparse (PurePattern Object dom var (Valid Object))
        , Traversable dom
        )
    => PurePattern Object dom var (Valid Object)
    -> PurePattern Object dom var (Valid Object)
    -> PurePattern Object dom var (Valid Object)
mkRewrites rewritesFirst rewritesSecond =
    asPurePattern (valid :< RewritesPattern rewrites0)
  where
    patternSort = unifySorts rewritesFirst rewritesSecond
    valid = Valid { patternSort }
    rewrites0 =
        Rewrites { rewritesSort = patternSort, rewritesFirst, rewritesSecond }

mkTop
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl -> PurePattern lvl dom var (Valid lvl)
mkTop patternSort =
    asPurePattern (Valid { patternSort } :< TopPattern top)
  where
    top = Top { topSort = patternSort }

mkVar
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl
    -> var lvl
    -> PurePattern lvl dom var (Valid lvl)
mkVar patternSort var =
    asPurePattern (valid :< VariablePattern var)
  where
    valid = Valid { patternSort }

mkCommonVar
    :: (Functor dom, MetaOrObject lvl)
    => Variable lvl
    -> PurePattern lvl dom Variable (Valid lvl)
mkCommonVar var =
    asPurePattern (valid :< VariablePattern var)
  where
    Variable { variableSort = patternSort } = var
    valid = Valid { patternSort }

mkVar'
    :: (Functor dom, MetaOrObject lvl)
    => Sort lvl
    -> (Sort lvl -> var lvl)
    -> PurePattern lvl dom var (Valid lvl)
mkVar' patternSort var =
    asPurePattern (valid :< VariablePattern (var patternSort))
  where
    valid = Valid { patternSort }

mkStringLiteral :: Functor dom => String -> PurePattern Meta dom var ()
mkStringLiteral string =
    asPurePattern (mempty :< StringLiteralPattern stringLiteral)
  where
    stringLiteral = StringLiteral string

mkCharLiteral :: Functor dom => Char -> PurePattern Meta dom var ()
mkCharLiteral char =
    asPurePattern (mempty :< CharLiteralPattern charLiteral)
  where
    charLiteral = CharLiteral char

mkSort
  :: MetaOrObject level
  => Text
  -> Sort level
mkSort name =
    SortActualSort $ SortActual (noLocationId name) []

-- | Construct a variable with a given name and sort
-- "x" `varS` s
varS :: MetaOrObject level => Text -> Sort level -> Variable level
varS x s =
    Variable (noLocationId x) s

-- | Construct a symbol with a given name and input sorts
-- "mult" `symS` [s, s]
-- Since the return sort is only found in MetadataTools, this is
-- mostly useful for testing.
symS :: MetaOrObject level => Text -> [Sort level] -> SymbolOrAlias level
symS x s =
    SymbolOrAlias (noLocationId x) s

-- | Placeholder. Should never appear in output of 'mk' funcs
fixmeSort
    :: MetaOrObject level
    => Sort level
fixmeSort = mkSort "FIXME"
