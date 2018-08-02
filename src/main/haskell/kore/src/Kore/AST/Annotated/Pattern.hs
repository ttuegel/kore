{-|
Module      : Kore.AST.Annotated.Pattern
Description : Annotate parts of patterns
Copyright   : (c) Runtime Verification, 2018
License     : UIUC/NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified. The type and constructor names
intentionally match "Kore.AST.Common" for symmetry.

The 'Pattern' type affords detailed annotation of all parts of a pattern,
compared to the other modules in @Kore.AST.Annotated@, which allow only a
pattern and its subterms to be annotated.

-}
module Kore.AST.Annotated.Pattern where

{- | Detailed annotation of all parts of a 'Kore.AST.Common.Pattern'

Although we can annotate a 'Kore.AST.Common.Pattern' using
'Control.Comonad.Cofree.Cofree' (for example), this only allows us to annotate
patterns and their subterms; we cannot annotate /parts/ of patterns such as the
individual sort parameters of a symbol application.

Each pattern carries an annotation covering the entire term. Patterns with
substructure carry specialized data annotating their parts.

-}
data Pattern ann =
      AndPattern !ann !(And ann)
    | ApplicationPattern !ann !(Application ann)
    | BottomPattern !ann !(Bottom ann)
    | CeilPattern !ann !(Ceil ann)
    | DomainValuePattern !ann !(DomainValue ann)
    | EqualsPattern !ann !(Equals ann)
    | ExistsPattern !ann !(Exists ann)
    | FloorPattern !ann !(Floor ann)
    | ForallPattern !ann !(Forall ann)
    | IffPattern !ann !(Iff ann)
    | ImpliesPattern !ann !(Implies ann)
    | InPattern !ann !(In ann)
    | NextPattern !ann !(Next ann)
    | NotPattern !ann !(Not ann)
    | OrPattern !ann !(Or ann)
    | RewritesPattern !ann !(Rewrites ann)
    | StringLiteralPattern !ann
      -- ^ Annotation of a literal string pattern
      -- ('Kore.AST.Common.StringLiteral'), which has no structure.
    | CharLiteralPattern !ann
      -- ^ Annotation of a literal character pattern
      -- ('Kore.AST.Common.CharLiteral'), which has no structure.
    | TopPattern !ann !(Top ann)
    | VariablePattern !ann !(Variable ann)
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a variable pattern ('Kore.AST.Common.Variable') or binder

See also: 'Exists', 'Forall'
-}
data Variable ann =
    Variable
    { name :: !ann
    , sort :: !(Sort ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a symbol or alias head in an application pattern
('Kore.AST.Common.SymbolOrAlias')

See also: 'Application'
-}
data SymbolOrAlias ann =
    SymbolOrAlias
    { constructor :: !ann
    , parameters :: ![ann]
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a sort variable in a sort ('Kore.AST.Common.SortVariable')

See also: 'Sort'
-}
newtype SortVariable ann = SortVariable { getSortVariable :: ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a concrete sort ('Kore.AST.Common.SortActual')

See also: 'Sort'
-}
data SortActual ann =
    SortActual
    { constructor :: !ann
    , parameters :: ![ann]
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a sort, which may be a variable or a concrete sort
('Kore.AST.Common.Sort')

-}
data Sort ann =
      SortActualSort !(SortActual ann)
    | SortVariableSort !(SortVariable ann)
  deriving (Eq, Ord, Read, Show)

{- | Annotation of a matching logic conjunction ('Kore.AST.Common.And')

Only the 'Sort' is annotated; the operands are subterms carrying their own
annotations.
-}
newtype And ann = And { getAnd :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of symbol or alias application ('Kore.AST.Common.Application')

Only the symbol or alias head is annotated; the arguments are subterms carrying
their own annotations.
-}
newtype Application ann = Application { getApplication :: SymbolOrAlias ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic bottom ('Kore.AST.Common.Bottom') -}
newtype Bottom ann = Bottom { getBottom :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic definedness ('Kore.AST.Common.Ceil')

Only the sorts are annotated; the operand is a subterm carrying its own
annotation.
-}
data Ceil ann =
    Ceil
    { operandSort :: !(Sort ann)
    , resultSort :: !(Sort ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of domain values ('Kore.AST.Common.DomainValue')

Only the sort is annotated; the argument does not carry any annotation.
-}
newtype DomainValue ann = DomainValue { getDomainValue :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic equality ('Kore.AST.Common.Equals')

Only the sorts are annotated; the operands are subterms carrying their own
annotations.
-}
data Equals ann =
    Equals
    { operandSort :: !(Sort ann)
    , resultSort :: !(Sort ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of existential quantification ('Kore.AST.Common.Exists')

Only the binder and result sort are annotated; the body is a subterm carrying
its own annotation.
-}
data Exists ann =
    Exists
    { resultSort :: !(Sort ann)
    , variable :: !(Variable ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic totality ('Kore.AST.Common.Floor')

Only the sorts are annotated; the operand is a subterm carrying its own
annotation.
-}
data Floor ann =
    Floor
    { operandSort :: !(Sort ann)
    , resultSort :: !(Sort ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of universal quantification ('Kore.AST.Common.Forall')

Only the binder and result sort are annotated; the body is a subterm carrying
its own annotation.
-}
data Forall ann =
    Forall
    { resultSort :: !(Sort ann)
    , variable :: !(Variable ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic if-and-only-if ('Kore.AST.Common.Iff')

Only the sort is annotated; the operands are subterms carrying their own
annotations.
-}
newtype Iff ann = Iff { getIff :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic implication ('Kore.AST.Common.Implies')

Only the sort is annotated; the operands are subterms carrying their own
annotations.
-}
newtype Implies ann = Implies { getImplies :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic membership ('Kore.AST.Common.In')

Only the sorts are annotated; the operands are subterms carrying their own
annotations.
-}
data In ann =
    In
    { operandSort :: !(Sort ann)
    , resultSort :: !(Sort ann)
    }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic "next" ('Kore.AST.Common.Next')

Only the sort is annotated; the operand is a subterm carrying its own
annotation.
-}
newtype Next ann = Next { getNext :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic negation ('Kore.AST.Common.Not')

Only the sort is annotated; the operand is a subterm carrying its own
annotation.
-}
newtype Not ann = Not { getNot :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic disjunction ('Kore.AST.Common.Or')

Only the sort is annotated; the operands are subterms carrying their own
annotations.
-}
newtype Or ann = Or { getOr :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic "rewrites" ('Kore.AST.Common.Rewrites')

Only the sort is annotated; the operands are subterms carrying their own
annotations.
-}
newtype Rewrites ann = Rewrites { getRewrites :: Sort ann }
  deriving (Eq, Ord, Read, Show)

{- | Annotation of matching logic top ('Kore.AST.Common.Top') -}
newtype Top ann = Top { getTop :: Sort ann }
  deriving (Eq, Ord, Read, Show)
