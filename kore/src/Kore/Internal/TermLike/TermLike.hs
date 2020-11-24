{-|
Copyright   : (c) Runtime Verification, 2019-2020
License     : NCSA

-}

{-# LANGUAGE UndecidableInstances #-}

module Kore.Internal.TermLike.TermLike
    ( Builtin
    , Evaluated (..)
    , Defined (..)
    , TermLike (..)
    , TermLikeF (..)
    , extractAttributes
    , freeVariables
    , mapVariables
    , traverseVariables
    , mkVar
    , traverseVariablesF
    , updateCallStack
    , depth
    ) where

import Prelude.Kore

import Control.Comonad.Trans.Cofree
    ( tailF
    )
import Control.DeepSeq
    ( NFData (..)
    )
import Control.Lens
    ( Lens'
    )
import qualified Control.Lens as Lens
import qualified Control.Monad.Reader as Reader
import Data.Functor.Const
    ( Const (..)
    )
import Data.Functor.Foldable
    ( Base
    , Corecursive
    , Recursive
    )
import qualified Data.Functor.Foldable as Recursive
import Data.Functor.Identity
    ( Identity (..)
    )
import qualified Data.Generics.Product as Lens.Product
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC
import qualified GHC.Stack as GHC

import Kore.AST.AstWithLocation
import qualified Kore.Attribute.Pattern as Attribute
import Kore.Attribute.Pattern.ConstructorLike
    ( HasConstructorLike (extractConstructorLike)
    )
import qualified Kore.Attribute.Pattern.ConstructorLike as Pattern
import Kore.Attribute.Pattern.Created
import qualified Kore.Attribute.Pattern.Defined as Pattern
import Kore.Attribute.Pattern.FreeVariables as FreeVariables
import qualified Kore.Attribute.Pattern.Function as Pattern
import qualified Kore.Attribute.Pattern.Functional as Pattern
import qualified Kore.Attribute.Pattern.Simplified as Pattern
import qualified Kore.Attribute.Pattern.Simplified as Simplified
    ( unparseTag
    )
import Kore.Attribute.Synthetic
import Kore.Builtin.Endianness.Endianness
    ( Endianness
    )
import Kore.Builtin.Signedness.Signedness
    ( Signedness
    )
import Kore.Debug
import qualified Kore.Domain.Builtin as Domain
import Kore.Internal.Alias
import Kore.Internal.Inj
import Kore.Internal.InternalBytes
import Kore.Internal.InternalInt
import Kore.Internal.Symbol hiding
    ( isConstructorLike
    )
import Kore.Internal.TermLike.Renaming
import Kore.Internal.Variable
import Kore.Sort
import Kore.Syntax.And
import Kore.Syntax.Application
import Kore.Syntax.Bottom
import Kore.Syntax.Ceil
import Kore.Syntax.DomainValue
import Kore.Syntax.Equals
import Kore.Syntax.Exists
import Kore.Syntax.Floor
import Kore.Syntax.Forall
import Kore.Syntax.Iff
import Kore.Syntax.Implies
import Kore.Syntax.In
import Kore.Syntax.Inhabitant
import Kore.Syntax.Mu
import Kore.Syntax.Next
import Kore.Syntax.Not
import Kore.Syntax.Nu
import Kore.Syntax.Or
import Kore.Syntax.Rewrites
import Kore.Syntax.StringLiteral
import Kore.Syntax.Top
import Kore.TopBottom
import Kore.Unparser
    ( Unparse (..)
    )
import qualified Kore.Unparser as Unparser
import Kore.Variables.Binding
import qualified Pretty
import qualified SQL

{- | @Evaluated@ wraps patterns which are fully evaluated.

Fully-evaluated patterns will not be simplified further because no progress
could be made.

 -}
newtype Evaluated child = Evaluated { getEvaluated :: child }
    deriving (Eq, Ord, Show)
    deriving (Foldable, Functor, Traversable)
    deriving (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (Evaluated child) where
    unparse evaluated =
        Pretty.vsep ["/* evaluated: */", Unparser.unparseGeneric evaluated]
    unparse2 evaluated =
        Pretty.vsep ["/* evaluated: */", Unparser.unparse2Generic evaluated]

instance Synthetic syn Evaluated where
    synthetic = getEvaluated
    {-# INLINE synthetic #-}

instance {-# OVERLAPS #-} Synthetic Pattern.Simplified Evaluated where
    synthetic = const Pattern.fullySimplified
    {-# INLINE synthetic #-}

{- | @Defined@ wraps patterns which are defined.

This avoids re-checking the definedness of terms which are already
known to be defined.

 -}
newtype Defined child = Defined { getDefined :: child }
    deriving (Eq, Ord, Show)
    deriving (Foldable, Functor, Traversable)
    deriving (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance Unparse child => Unparse (Defined child) where
    unparse defined =
        Pretty.vsep ["/* defined: */", Unparser.unparseGeneric defined]
    unparse2 defined =
        Pretty.vsep ["/* defined: */", Unparser.unparse2Generic defined]

instance Synthetic syn Defined where
    synthetic = getDefined
    {-# INLINE synthetic #-}

instance {-# OVERLAPS #-} Synthetic Pattern.Defined Defined where
    synthetic = const (Pattern.Defined True)
    {-# INLINE synthetic #-}

{- | 'TermLikeF' is the 'Base' functor of internal term-like patterns.

-}
data TermLikeF variable child
    = AndF           !(And Sort child)
    | ApplySymbolF   !(Application Symbol child)
    | ApplyAliasF    !(Application (Alias (TermLike VariableName)) child)
    | BottomF        !(Bottom Sort child)
    | CeilF          !(Ceil Sort child)
    | DomainValueF   !(DomainValue Sort child)
    | EqualsF        !(Equals Sort child)
    | ExistsF        !(Exists Sort variable child)
    | FloorF         !(Floor Sort child)
    | ForallF        !(Forall Sort variable child)
    | IffF           !(Iff Sort child)
    | ImpliesF       !(Implies Sort child)
    | InF            !(In Sort child)
    | MuF            !(Mu variable child)
    | NextF          !(Next Sort child)
    | NotF           !(Not Sort child)
    | NuF            !(Nu variable child)
    | OrF            !(Or Sort child)
    | RewritesF      !(Rewrites Sort child)
    | TopF           !(Top Sort child)
    | InhabitantF    !(Inhabitant child)
    | BuiltinF       !(Builtin child)
    | InternalIntF   !(Const InternalInt child)
    | EvaluatedF     !(Evaluated child)
    | StringLiteralF !(Const StringLiteral child)
    | InternalBytesF !(Const InternalBytes child)
    | VariableF      !(Const (SomeVariable variable) child)
    | EndiannessF    !(Const Endianness child)
    | SignednessF    !(Const Signedness child)
    | InjF           !(Inj child)
    | DefinedF       !(Defined child)
    deriving (Eq, Ord, Show)
    deriving (Foldable, Functor, Traversable)
    deriving (GHC.Generic)
    deriving anyclass (Hashable, NFData)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug, Diff)

instance
    (Unparse variable, Unparse child) => Unparse (TermLikeF variable child)
  where
    unparse = Unparser.unparseGeneric
    unparse2 = Unparser.unparse2Generic

instance
    Ord variable => Synthetic (FreeVariables variable) (TermLikeF variable)
  where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Sort (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Pattern.Functional (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Pattern.Function (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Pattern.Defined (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Pattern.Simplified (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

instance Synthetic Pattern.ConstructorLike (TermLikeF variable) where
    synthetic =
        \case
            AndF and' -> synthetic and'
            ApplySymbolF application -> synthetic application
            ApplyAliasF application -> synthetic application
            BottomF bottom -> synthetic bottom
            CeilF ceil -> synthetic ceil
            DomainValueF domainValue -> synthetic domainValue
            EqualsF equals -> synthetic equals
            ExistsF exists -> synthetic exists
            FloorF floor' -> synthetic floor'
            ForallF forall' -> synthetic forall'
            IffF iff -> synthetic iff
            ImpliesF implies -> synthetic implies
            InF in' -> synthetic in'
            MuF mu -> synthetic mu
            NextF next -> synthetic next
            NotF not' -> synthetic not'
            NuF nu -> synthetic nu
            OrF or' -> synthetic or'
            RewritesF rewrites -> synthetic rewrites
            TopF top -> synthetic top
            InhabitantF inhabitant -> synthetic inhabitant
            BuiltinF builtin -> synthetic builtin
            EvaluatedF evaluated -> synthetic evaluated
            StringLiteralF stringLiteral -> synthetic stringLiteral
            InternalBytesF internalBytes -> synthetic internalBytes
            InternalIntF internalInt -> synthetic internalInt
            VariableF variable -> synthetic variable
            EndiannessF endianness -> synthetic endianness
            SignednessF signedness -> synthetic signedness
            InjF inj -> synthetic inj
            DefinedF defined -> synthetic defined

{- | @TermLike@ is a term-like Kore pattern.

@TermLike@ is the common internal representation of patterns, especially terms.

@TermLike@ is essentially 'Control.Comonad.Cofree.Cofree', but rather than
define a @newtype@ over @Cofree@, it is defined inline for performance. The
performance advantage owes to the fact that the instances of 'Recursive.project'
and 'Recursive.embed' correspond to unwrapping and wrapping the @newtype@,
respectively, which is free at runtime.

 -}
newtype TermLike variable =
    TermLike
        { getTermLike
            ::  CofreeF
                    (TermLikeF variable)
                    (Attribute.Pattern variable)
                    (TermLike variable)
        }
    deriving (Show)
    deriving (GHC.Generic)
    deriving anyclass (SOP.Generic, SOP.HasDatatypeInfo)
    deriving anyclass (Debug)

instance (Debug variable, Diff variable) => Diff (TermLike variable) where
    diffPrec
        termLike1@(Recursive.project -> attrs1 :< termLikeF1)
        termLike2@(Recursive.project -> _      :< termLikeF2)
      =
        -- If the patterns differ, do not display the difference in the
        -- attributes, which would overload the user with redundant information.
        diffPrecGeneric
            (Recursive.embed (attrs1 :< termLikeF1))
            (Recursive.embed (attrs1 :< termLikeF2))
        <|> diffPrecGeneric termLike1 termLike2

instance
    (Eq variable, Eq (TermLikeF variable (TermLike variable)))
    => Eq (TermLike variable)
  where
    (==)
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2)
      = pat1 == pat2

instance
    (Ord variable, Ord (TermLikeF variable (TermLike variable)))
    => Ord (TermLike variable)
  where
    compare
        (Recursive.project -> _ :< pat1)
        (Recursive.project -> _ :< pat2)
      = compare pat1 pat2

instance Hashable variable => Hashable (TermLike variable) where
    hashWithSalt salt (Recursive.project -> _ :< pat) = hashWithSalt salt pat
    {-# INLINE hashWithSalt #-}

instance NFData variable => NFData (TermLike variable) where
    rnf (Recursive.project -> annotation :< pat) =
        rnf annotation `seq` rnf pat

instance (Unparse variable, Ord variable) => Unparse (TermLike variable) where
    unparse term =
        case Recursive.project term of
            (attrs :< termLikeF)
              | hasKnownCreator created ->
                Pretty.sep
                    [ Pretty.pretty created
                    , attributeRepresentation
                    , unparse termLikeF
                    ]
              | otherwise ->
                Pretty.sep [attributeRepresentation, unparse termLikeF]
              where
                Attribute.Pattern { created } = attrs

                attributeRepresentation = case attrs of
                    (Attribute.Pattern _ _ _ _ _ _ _ _) ->
                        Pretty.surround
                            (Pretty.hsep $ map Pretty.pretty representation)
                            "/* "
                            " */"
                  where
                    representation =
                        addFunctionalRepresentation
                        $ addFunctionRepresentation
                        $ addDefinedRepresentation
                        $ addSimplifiedRepresentation
                        $ addConstructorLikeRepresentation []
                addFunctionalRepresentation
                  | Pattern.isFunctional $ Attribute.functional attrs = ("Fl" :)
                  | otherwise = id
                addFunctionRepresentation
                  | Pattern.isFunction $ Attribute.function attrs = ("Fn" :)
                  | otherwise = id
                addDefinedRepresentation
                  | Pattern.isDefined $ Attribute.defined attrs = ("D" :)
                  | otherwise = id
                addSimplifiedRepresentation =
                    case simplifiedTag of
                        Just result -> (result :)
                        Nothing -> id
                  where
                    simplifiedTag =
                        Simplified.unparseTag
                            (Attribute.simplifiedAttribute attrs)
                addConstructorLikeRepresentation =
                    case constructorLike of
                        Just Pattern.ConstructorLikeHead -> ("Cl" :)
                        Just Pattern.SortInjectionHead -> ("Cli" :)
                        Nothing -> id
                  where
                    constructorLike =
                        Pattern.getConstructorLike
                            (Attribute.constructorLikeAttribute attrs)

    unparse2 term =
        case Recursive.project term of
          (_ :< pat) -> unparse2 pat

type instance Base (TermLike variable) =
    CofreeF (TermLikeF variable) (Attribute.Pattern variable)

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Recursive (TermLike variable) where
    project = getTermLike
    {-# INLINE project #-}

-- This instance implements all class functions for the TermLike newtype
-- because the their implementations for the inner type may be specialized.
instance Corecursive (TermLike variable) where
    embed = TermLike
    {-# INLINE embed #-}

instance TopBottom (TermLike variable) where
    isTop (Recursive.project -> _ :< TopF Top {}) = True
    isTop _ = False
    isBottom (Recursive.project -> _ :< BottomF Bottom {}) = True
    isBottom _ = False

instance InternalVariable variable => Binding (TermLike variable) where
    type VariableType (TermLike variable) = variable

    traverseVariable traversal termLike =
        case termLikeF of
            VariableF (Const unifiedVariable) ->
                mkVar <$> traversal unifiedVariable
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

    traverseSetBinder traversal termLike =
        case termLikeF of
            MuF mu -> synthesize . MuF <$> muBinder traversal mu
            NuF nu -> synthesize . NuF <$> nuBinder traversal nu
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

    traverseElementBinder traversal termLike =
        case termLikeF of
            ExistsF exists ->
                synthesize . ExistsF <$> existsBinder traversal exists
            ForallF forall ->
                synthesize . ForallF <$> forallBinder traversal forall
            _ -> pure termLike
      where
        _ :< termLikeF = Recursive.project termLike

instance HasConstructorLike (TermLike variable) where
    extractConstructorLike (Recursive.project -> attrs :< _) =
        extractConstructorLike attrs

instance Unparse (TermLike variable) => SQL.Column (TermLike variable) where
    defineColumn = SQL.defineTextColumn
    toColumn = SQL.toColumn . Pretty.renderText . Pretty.layoutOneLine . unparse

instance
    (FreshPartialOrd variable)
    => From (TermLike Concrete) (TermLike variable)
  where
    from = mapVariables (pure $ from @Concrete)
    {-# INLINE from #-}

-- | The type of internal domain values.
type Builtin = Domain.Builtin (TermLike Concrete)

instance
    ( AstWithLocation variable
    , AstWithLocation child
    ) =>
    AstWithLocation (TermLikeF variable child)
  where
    locationFromAst =
        \case
            AndF And { andSort } -> locationFromAst andSort
            ApplySymbolF Application { applicationSymbolOrAlias } ->
                locationFromAst applicationSymbolOrAlias
            ApplyAliasF Application { applicationSymbolOrAlias } ->
                locationFromAst applicationSymbolOrAlias
            BottomF Bottom { bottomSort } -> locationFromAst bottomSort
            CeilF Ceil { ceilResultSort } -> locationFromAst ceilResultSort
            DomainValueF domain -> locationFromAst $ domainValueSort domain
            EqualsF Equals { equalsResultSort } ->
                locationFromAst equalsResultSort
            ExistsF Exists { existsSort } -> locationFromAst existsSort
            FloorF Floor { floorResultSort } ->
                locationFromAst floorResultSort
            ForallF Forall { forallSort } -> locationFromAst forallSort
            IffF Iff { iffSort } -> locationFromAst iffSort
            ImpliesF Implies { impliesSort } ->
                locationFromAst impliesSort
            InF In { inResultSort } -> locationFromAst inResultSort
            MuF Mu { muVariable } -> locationFromAst muVariable
            NextF Next { nextSort } -> locationFromAst nextSort
            NotF Not { notSort } -> locationFromAst notSort
            NuF Nu { nuVariable } -> locationFromAst nuVariable
            OrF Or { orSort } -> locationFromAst orSort
            RewritesF Rewrites { rewritesSort } ->
                locationFromAst rewritesSort
            StringLiteralF _ -> AstLocationUnknown
            TopF Top { topSort } -> locationFromAst topSort
            VariableF (Const variable) -> locationFromAst variable
            InhabitantF Inhabitant { inhSort } -> locationFromAst inhSort
            EvaluatedF Evaluated { getEvaluated } ->
                locationFromAst getEvaluated
            InjF Inj { injChild } -> locationFromAst injChild
            SignednessF (Const signedness) -> locationFromAst signedness
            EndiannessF (Const endianness) -> locationFromAst endianness
            InternalBytesF (Const InternalBytes { bytesSort }) ->
                locationFromAst bytesSort
            BuiltinF builtin -> locationFromAst (Domain.builtinSort builtin)
            InternalIntF (Const InternalInt { internalIntSort }) ->
                locationFromAst internalIntSort
            DefinedF Defined { getDefined } ->
                locationFromAst getDefined

instance AstWithLocation variable => AstWithLocation (TermLike variable)
  where
    locationFromAst = locationFromAst . tailF . Recursive.project

{- | Use the provided traversal to replace all variables in a 'TermLikeF' head.

__Warning__: @traverseVariablesF@ will capture variables if the provided
traversal is not injective!

-}
traverseVariablesF
    :: Applicative f
    => AdjSomeVariableName (variable1 -> f variable2)
    -> TermLikeF variable1 child
    -> f (TermLikeF variable2 child)
traverseVariablesF adj =
    \case
        -- Non-trivial cases
        ExistsF any0 -> ExistsF <$> traverseVariablesExists any0
        ForallF all0 -> ForallF <$> traverseVariablesForall all0
        MuF any0 -> MuF <$> traverseVariablesMu any0
        NuF any0 -> NuF <$> traverseVariablesNu any0
        VariableF variable -> VariableF <$> traverseConstVariable variable
        -- Trivial cases
        AndF andP -> pure (AndF andP)
        ApplySymbolF applySymbolF -> pure (ApplySymbolF applySymbolF)
        ApplyAliasF applyAliasF -> pure (ApplyAliasF applyAliasF)
        BottomF botP -> pure (BottomF botP)
        BuiltinF builtinP -> pure (BuiltinF builtinP)
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
        InternalBytesF bytesP -> pure (InternalBytesF bytesP)
        InternalIntF bytesP -> pure (InternalIntF bytesP)
        TopF topP -> pure (TopF topP)
        InhabitantF s -> pure (InhabitantF s)
        EvaluatedF childP -> pure (EvaluatedF childP)
        EndiannessF endianness -> pure (EndiannessF endianness)
        SignednessF signedness -> pure (SignednessF signedness)
        InjF inj -> pure (InjF inj)
        DefinedF childP -> pure (DefinedF childP)
  where
    trElemVar = traverse $ traverseElementVariableName adj
    trSetVar = traverse $ traverseSetVariableName adj
    traverseConstVariable (Const variable) =
        Const <$> traverseSomeVariable adj variable
    traverseVariablesExists Exists { existsSort, existsVariable, existsChild } =
        Exists existsSort
        <$> trElemVar existsVariable
        <*> pure existsChild
    traverseVariablesForall Forall { forallSort, forallVariable, forallChild } =
        Forall forallSort
        <$> trElemVar forallVariable
        <*> pure forallChild
    traverseVariablesMu Mu { muVariable, muChild } =
        Mu <$> trSetVar muVariable <*> pure muChild
    traverseVariablesNu Nu { nuVariable, nuChild } =
        Nu <$> trSetVar nuVariable <*> pure nuChild

extractAttributes :: TermLike variable -> Attribute.Pattern variable
extractAttributes (TermLike (attrs :< _)) = attrs

instance HasFreeVariables (TermLike variable) variable where
    freeVariables = Attribute.freeVariables . extractAttributes

{- | Use the provided mapping to replace all variables in a 'StepPattern'.

@mapVariables@ is lazy: it descends into its argument only as the result is
demanded. Intermediate allocation from composing multiple transformations with
@mapVariables@ is amortized; the intermediate trees are never fully resident.

See also: 'traverseVariables'

 -}
mapVariables
    :: forall variable1 variable2
    .  Ord variable1
    => FreshPartialOrd variable2
    => AdjSomeVariableName (variable1 -> variable2)
    -> TermLike variable1
    -> TermLike variable2
mapVariables adj termLike =
    runIdentity (traverseVariables ((.) pure <$> adj) termLike)
{-# INLINE mapVariables #-}

{- | Use the provided traversal to replace all variables in a 'TermLike'.

@traverseVariables@ is strict, i.e. its argument is fully evaluated before it
returns. When composing multiple transformations with @traverseVariables@, the
intermediate trees will be fully allocated; @mapVariables@ is more composable in
this respect.

See also: 'mapVariables'

 -}
traverseVariables
    :: forall variable1 variable2 m
    .  Ord variable1
    => FreshPartialOrd variable2
    => Monad m
    => AdjSomeVariableName (variable1 -> m variable2)
    -> TermLike variable1
    -> m (TermLike variable2)
traverseVariables adj termLike =
    renameFreeVariables adj (freeVariables @_ @variable1 termLike)
    >>= Reader.runReaderT (Recursive.fold worker termLike)
  where
    adjReader = (.) lift <$> adj
    trElemVar = traverse $ traverseElementVariableName adjReader
    trSetVar = traverse $ traverseSetVariableName adjReader
    traverseExists avoiding =
        existsBinder (renameElementBinder trElemVar avoiding)
    traverseForall avoiding =
        forallBinder (renameElementBinder trElemVar avoiding)
    traverseMu avoiding =
        muBinder (renameSetBinder trSetVar avoiding)
    traverseNu avoiding =
        nuBinder (renameSetBinder trSetVar avoiding)

    worker
        ::  Base
                (TermLike variable1)
                (RenamingT variable1 variable2 m (TermLike variable2))
        ->  RenamingT variable1 variable2 m (TermLike variable2)
    worker (attrs :< termLikeF) = do
        attrs' <- Attribute.traverseVariables askSomeVariableName attrs
        let avoiding = freeVariables attrs'
        termLikeF' <- case termLikeF of
            VariableF (Const unifiedVariable) -> do
                unifiedVariable' <- askSomeVariable unifiedVariable
                (pure . VariableF) (Const unifiedVariable')
            ExistsF exists -> ExistsF <$> traverseExists avoiding exists
            ForallF forall -> ForallF <$> traverseForall avoiding forall
            MuF mu -> MuF <$> traverseMu avoiding mu
            NuF nu -> NuF <$> traverseNu avoiding nu
            _ ->
                sequence termLikeF >>=
                -- traverseVariablesF will not actually call the traversals
                -- because all the cases with variables are handled above.
                traverseVariablesF askSomeVariableName
        (pure . Recursive.embed) (attrs' :< termLikeF')

updateCallStack
    :: forall variable
    .  HasCallStack
    => TermLike variable
    -> TermLike variable
updateCallStack = Lens.set created callstack
  where
    created = _attributes . Lens.Product.field @"created"
    callstack =
        Created . Just . GHC.popCallStack . GHC.popCallStack $ GHC.callStack

    _attributes :: Lens' (TermLike variable) (Attribute.Pattern variable)
    _attributes =
        Lens.lens
            (\(TermLike (attrs :< _)) -> attrs)
            (\(TermLike (_ :< termLikeF)) attrs ->
                TermLike (attrs :< termLikeF)
            )

{- | Construct a variable pattern.
 -}
mkVar
    :: HasCallStack
    => Ord variable
    => SomeVariable variable
    -> TermLike variable
mkVar = updateCallStack . synthesize . VariableF . Const

depth :: TermLike variable -> Int
depth = Recursive.fold levelDepth
  where
    levelDepth (_ :< termF) = 1 + foldl' max 0 termF
