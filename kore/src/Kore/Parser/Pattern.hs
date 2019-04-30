{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA

-}

{-# LANGUAGE TemplateHaskell #-}

module Kore.Parser.Pattern
    ( Pattern (..)
    , PatternF (..)
    , mapVariablesF
    , traverseVariablesF
    , mapVariables
    , asPattern
    , asPatternBase
    , freeVariables
    -- * Pattern synonyms
    , pattern StringLiteral_
    -- * Re-exports
    , module Kore.Syntax
    ) where

import           Control.Comonad.Trans.Cofree
import qualified Control.Comonad.Trans.Env as Env
import           Control.DeepSeq
                 ( NFData (..) )
import qualified Control.Monad as Monad
import qualified Control.Monad.RWS.Strict as Monad.RWS
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Deriving as Deriving
import           Data.Functor.Classes
import           Data.Functor.Compose
                 ( Compose (..) )
import           Data.Functor.Foldable
                 ( Base, Corecursive, Recursive )
import qualified Data.Functor.Foldable as Recursive
import           Data.Functor.Identity
                 ( Identity (..) )
import           Data.Hashable
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text
                 ( Text )
import           GHC.Generics
                 ( Generic )

import Kore.AST.AstWithLocation
import Kore.Syntax
import Kore.TopBottom
import Kore.Unparser

{- | @PatternF@ is the head of a Kore pattern.

See /The Semantics of K/, Section 9.1.4 (Patterns).
-}
data PatternF variable child
    = AndF !(And Sort child)
    | ApplicationF !(Application SymbolOrAlias child)
    | BottomF !(Bottom Sort child)
    | CeilF !(Ceil Sort child)
    | DomainValueF !(DomainValue Sort child)
    | EqualsF !(Equals Sort child)
    | ExistsF !(Exists Sort variable child)
    | FloorF !(Floor Sort child)
    | ForallF !(Forall Sort variable child)
    | IffF !(Iff Sort child)
    | ImpliesF !(Implies Sort child)
    | InF !(In Sort child)
    | NextF !(Next Sort child)
    | NotF !(Not Sort child)
    | OrF !(Or Sort child)
    | RewritesF !(Rewrites Sort child)
    | StringLiteralF !StringLiteral
    | CharLiteralF !CharLiteral
    | TopF !(Top Sort child)
    | VariableF !variable
    | InhabitantF !Sort
    | SetVariableF !(SetVariable variable)
    deriving (Eq, Foldable, Functor, Generic, Ord, Show, Traversable)

Deriving.deriveEq1 ''PatternF
Deriving.deriveOrd1 ''PatternF
Deriving.deriveShow1 ''PatternF

instance
    (Hashable child, Hashable variable) =>
    Hashable (PatternF variable child)

instance (NFData child, NFData variable) => NFData (PatternF variable child)

instance (Unparse child, Unparse variable) => Unparse (PatternF variable child)
  where
    unparse =
        \case
            AndF p           -> unparse p
            ApplicationF p   -> unparse p
            BottomF p        -> unparse p
            CeilF p          -> unparse p
            DomainValueF p   -> unparse p
            EqualsF p        -> unparse p
            ExistsF p        -> unparse p
            FloorF p         -> unparse p
            ForallF p        -> unparse p
            IffF p           -> unparse p
            ImpliesF p       -> unparse p
            InF p            -> unparse p
            NextF p          -> unparse p
            NotF p           -> unparse p
            OrF p            -> unparse p
            RewritesF p      -> unparse p
            StringLiteralF p -> unparse p
            CharLiteralF p   -> unparse p
            TopF p           -> unparse p
            VariableF p      -> unparse p
            InhabitantF s    -> unparse s
            SetVariableF p   -> unparse p

    unparse2 =
        \case
            AndF p           -> unparse2 p
            ApplicationF p   -> unparse2 p
            BottomF p        -> unparse2 p
            CeilF p          -> unparse2 p
            DomainValueF p   -> unparse2 p
            EqualsF p        -> unparse2 p
            ExistsF p        -> unparse2 p
            FloorF p         -> unparse2 p
            ForallF p        -> unparse2 p
            IffF p           -> unparse2 p
            ImpliesF p       -> unparse2 p
            InF p            -> unparse2 p
            NextF p          -> unparse2 p
            NotF p           -> unparse2 p
            OrF p            -> unparse2 p
            RewritesF p      -> unparse2 p
            StringLiteralF p -> unparse2 p
            CharLiteralF p   -> unparse2 p
            TopF p           -> unparse2 p
            VariableF p      -> unparse2 p
            InhabitantF s    -> unparse s
            SetVariableF p   -> unparse p

instance
    AstWithLocation variable =>
    AstWithLocation (PatternF variable child)
  where
    locationFromAst =
        \case
            AndF And { andSort } -> locationFromAst andSort
            ApplicationF Application { applicationSymbolOrAlias } ->
                locationFromAst applicationSymbolOrAlias
            BottomF Bottom { bottomSort } -> locationFromAst bottomSort
            CeilF Ceil { ceilResultSort } ->
                locationFromAst ceilResultSort
            DomainValueF DomainValue { domainValueSort } ->
                locationFromAst domainValueSort
            EqualsF Equals { equalsResultSort } ->
                locationFromAst equalsResultSort
            ExistsF Exists { existsSort } -> locationFromAst existsSort
            FloorF Floor { floorResultSort } ->
                locationFromAst floorResultSort
            ForallF Forall { forallSort } -> locationFromAst forallSort
            IffF Iff { iffSort } -> locationFromAst iffSort
            ImpliesF Implies { impliesSort } ->
                locationFromAst impliesSort
            InF In { inResultSort } ->
                locationFromAst inResultSort
            NextF Next { nextSort } -> locationFromAst nextSort
            NotF Not { notSort } -> locationFromAst notSort
            OrF Or { orSort } -> locationFromAst orSort
            RewritesF Rewrites { rewritesSort } ->
                locationFromAst rewritesSort
            StringLiteralF _ -> AstLocationUnknown
            CharLiteralF _ -> AstLocationUnknown
            TopF Top { topSort } -> locationFromAst topSort
            VariableF variable -> locationFromAst variable
            InhabitantF s -> locationFromAst s
            SetVariableF (SetVariable variable) ->
                locationFromAst variable

    updateAstLocation = undefined

{- | Use the provided mapping to replace all variables in a 'PatternF' head.

__Warning__: @mapVariablesF@ will capture variables if the provided mapping is
not injective!

-}
mapVariablesF
    :: (variable1 -> variable2)
    -> PatternF variable1 child
    -> PatternF variable2 child
mapVariablesF mapping =
    runIdentity . traverseVariablesF (Identity . mapping)
{-# INLINE mapVariablesF #-}

{- | Use the provided traversal to replace all variables in a 'PatternF' head.

__Warning__: @traverseVariablesF@ will capture variables if the provided
traversal is not injective!

-}
traverseVariablesF
    :: Applicative f
    => (variable1 -> f variable2)
    -> PatternF variable1 child
    -> f (PatternF variable2 child)
traverseVariablesF traversing =
    \case
        -- Non-trivial cases
        ExistsF any0 -> ExistsF <$> traverseVariablesExists any0
        ForallF all0 -> ForallF <$> traverseVariablesForall all0
        VariableF variable -> VariableF <$> traversing variable
        InhabitantF s -> pure (InhabitantF s)
        SetVariableF (SetVariable variable) ->
            SetVariableF . SetVariable <$> traversing variable
        -- Trivial cases
        AndF andP -> pure (AndF andP)
        ApplicationF appP -> pure (ApplicationF appP)
        BottomF botP -> pure (BottomF botP)
        CeilF ceilP -> pure (CeilF ceilP)
        DomainValueF dvP -> pure (DomainValueF dvP)
        EqualsF eqP -> pure (EqualsF eqP)
        FloorF flrP -> pure (FloorF flrP)
        IffF iffP -> pure (IffF iffP)
        ImpliesF impP -> pure (ImpliesF impP)
        InF inP -> pure (InF inP)
        NextF nxtP -> pure (NextF nxtP)
        NotF notP -> pure (NotF notP)
        OrF orP -> pure (OrF orP)
        RewritesF rewP -> pure (RewritesF rewP)
        StringLiteralF strP -> pure (StringLiteralF strP)
        CharLiteralF charP -> pure (CharLiteralF charP)
        TopF topP -> pure (TopF topP)
  where
    traverseVariablesExists Exists { existsSort, existsVariable, existsChild } =
        Exists existsSort <$> traversing existsVariable <*> pure existsChild
    traverseVariablesForall Forall { forallSort, forallVariable, forallChild } =
        Forall forallSort <$> traversing forallVariable <*> pure forallChild
{-# INLINE traverseVariablesF #-}

{- | The syntax of Kore patterns.

@variable@ is the type of variables.

-}
newtype Pattern variable =
    Pattern { getPattern :: Cofree (PatternF variable) () }
    deriving (Generic, Show)

-- | 'Eq' ignores annotations.
instance Eq variable => Eq (Pattern variable) where
    (==) = eqWorker
      where
        eqWorker
            (Recursive.project -> _ :< pat1)
            (Recursive.project -> _ :< pat2)
          =
            liftEq eqWorker pat1 pat2
    {-# INLINE (==) #-}

-- | 'Ord' ignores annotations.
instance Ord variable => Ord (Pattern variable) where
    compare = compareWorker
      where
        compareWorker
            (Recursive.project -> _ :< pat1)
            (Recursive.project -> _ :< pat2)
          =
            liftCompare compareWorker pat1 pat2
    {-# INLINE compare #-}

instance Hashable variable => Hashable (Pattern variable) where
    hashWithSalt salt (Recursive.project -> _ :< pat) = hashWithSalt salt pat
    {-# INLINE hashWithSalt #-}

instance NFData variable => NFData (Pattern variable) where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat `seq` ()

instance Unparse variable => Unparse (Pattern variable) where
    unparse (Recursive.project -> _ :< pat) = unparse pat
    unparse2 (Recursive.project -> _ :< pat) = unparse2 pat

type instance Base (Pattern variable) = CofreeF (PatternF variable) ()

-- This instance implements all class functions for the PurePattern newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive (Pattern variable) where
    project = \(Pattern embedded) ->
        case Recursive.project embedded of
            Compose (Identity projected) -> Pattern <$> projected
    {-# INLINE project #-}

    -- This specialization is particularly important: The default implementation
    -- of 'cata' in terms of 'project' would involve an extra call to 'fmap' at
    -- every level of the tree due to the implementation of 'project' above.
    cata alg = \(Pattern fixed) ->
        Recursive.cata
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE cata #-}

    para alg = \(Pattern fixed) ->
        Recursive.para
            (\(Compose (Identity base)) ->
                 alg (Bifunctor.first Pattern <$> base)
            )
            fixed
    {-# INLINE para #-}

    gpara dist alg = \(Pattern fixed) ->
        Recursive.gpara
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> alg (Env.local Pattern <$> base))
            fixed
    {-# INLINE gpara #-}

    prepro pre alg = \(Pattern fixed) ->
        Recursive.prepro
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE prepro #-}

    gprepro dist pre alg = \(Pattern fixed) ->
        Recursive.gprepro
            (\(Compose (Identity base)) -> Compose . Identity <$> dist base)
            (\(Compose (Identity base)) -> (Compose . Identity) (pre base))
            (\(Compose (Identity base)) -> alg base)
            fixed
    {-# INLINE gprepro #-}

-- This instance implements all class functions for the Pattern newtype
-- because the their implementations for the inner type may be specialized.
instance Corecursive (Pattern variable) where
    embed = \projected ->
        (Pattern . Recursive.embed . Compose . Identity)
            (getPattern <$> projected)
    {-# INLINE embed #-}

    ana coalg = Pattern . ana0
      where
        ana0 =
            Recursive.ana (Compose . Identity . coalg)
    {-# INLINE ana #-}

    apo coalg = Pattern . apo0
      where
        apo0 =
            Recursive.apo
                (\a ->
                     (Compose . Identity)
                        (Bifunctor.first getPattern <$> coalg a)
                )
    {-# INLINE apo #-}

    postpro post coalg = Pattern . postpro0
      where
        postpro0 =
            Recursive.postpro
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE postpro #-}

    gpostpro dist post coalg = Pattern . gpostpro0
      where
        gpostpro0 =
            Recursive.gpostpro
                (Compose . Identity . dist . (<$>) (runIdentity . getCompose))
                (\(Compose (Identity base)) -> (Compose . Identity) (post base))
                (Compose . Identity . coalg)
    {-# INLINE gpostpro #-}

instance TopBottom (Pattern variable) where
    isTop (Recursive.project -> _ :< TopF _) = True
    isTop _ = False

    isBottom (Recursive.project -> _ :< BottomF _) = True
    isBottom _ = False

{- | Use the provided mapping to replace all variables in a pattern.

__Warning__: @mapVariables@ will capture variables if the provided mapping is
not injective!

-}
mapVariables
    :: (variable1 -> variable2)
    -> Pattern variable1
    -> Pattern variable2
mapVariables mapping =
    Pattern . transCofreeT (mapVariablesF mapping) . getPattern
{-# INLINE mapVariables #-}

asPattern :: (PatternF variable) (Pattern variable) -> Pattern variable
asPattern = Recursive.embed . asPatternBase
{-# INLINE asPattern #-}

asPatternBase
    :: (PatternF variable) a
    -> Base (Pattern variable) a
asPatternBase patternBase = mempty :< patternBase
{-# INLINE asPatternBase #-}

-- | The free variables of a 'Pattern'.
freeVariables :: Ord variable => Pattern variable -> Set variable
freeVariables root =
    let (free, ()) =
            Monad.RWS.execRWS
                (freePureVariables1 root)
                Set.empty  -- initial set of bound variables
                Set.empty  -- initial set of free variables
    in
        free
  where
    unlessM m go = m >>= \b -> Monad.unless b go
    isBound v = Monad.RWS.asks (Set.member v)
    recordFree v = Monad.RWS.modify' (Set.insert v)

    freePureVariables1 (Recursive.project -> _ :< projected) =
        case projected of
            VariableF v -> unlessM (isBound v) (recordFree v)
            ExistsF Exists { existsVariable, existsChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert existsVariable)
                    -- descend into the bound pattern
                    (freePureVariables1 existsChild)
            ForallF Forall { forallVariable, forallChild } ->
                Monad.RWS.local
                    -- record the bound variable
                    (Set.insert forallVariable)
                    -- descend into the bound pattern
                    (freePureVariables1 forallChild)
            p -> mapM_ freePureVariables1 p

pattern StringLiteral_ :: Text -> Pattern variable
pattern StringLiteral_ literal <-
    (Recursive.project -> _ :< StringLiteralF (StringLiteral literal))
  where
    StringLiteral_ literal = asPattern (StringLiteralF $ StringLiteral literal)
