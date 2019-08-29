{-|
Module      : Kore.Step.SMT.AST
Description : Data types involved in declaring and using SMT symbols.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
-}
module Kore.Step.SMT.AST
  ( Declarations (..),
    Encodable (AlreadyEncoded),
    IndirectSymbolDeclaration (..),
    KoreSortDeclaration (..),
    KoreSymbolDeclaration (..),
    SmtDeclarations,
    SmtKoreSymbolDeclaration,
    SmtSort,
    SmtSymbol,
    Sort (..),
    SortReference (..),
    Symbol (..),
    SymbolReference (..),
    UnresolvedConstructor,
    UnresolvedConstructorArgument,
    UnresolvedDataTypeDeclaration,
    UnresolvedDeclarations,
    UnresolvedFunctionDeclaration,
    UnresolvedIndirectSymbolDeclaration,
    UnresolvedKoreSortDeclaration,
    UnresolvedKoreSymbolDeclaration,
    UnresolvedSort,
    UnresolvedSortDeclaration,
    UnresolvedSymbol,
    appendToEncoding,
    encodable,
    encode,
    mergePreferFirst
    )
where

import Data.Map.Strict
  ( Map
    )
import qualified Data.Map.Strict as Map
import Data.Text
  ( Text
    )
import qualified Kore.Sort as Kore
  ( Sort
    )
import Kore.Step.SMT.Encoder
  ( encodeName
    )
import qualified Kore.Syntax.Id as Kore
  ( Id (Id, getId)
    )
import qualified SMT.AST as AST

{-| A representation of the Kore Sort type together with its related
declarations (constructors, noJunk axioms), optimized for dealing with the SMT.

The SmtSort type below instantiates Sort with the types used by the SMT.
-}
data Sort sort symbol name
  = Sort
      { smtFromSortArgs
          :: !(Map Kore.Id SmtSort -> [Kore.Sort] -> Maybe AST.SExpr),
        -- ^ Produces the SMT representation of a sort. Given a map with
        -- Smt representations for sorts and a list of sort arguments, returns
        -- an S-expression that can be used, say, when declaring symbols of
        -- that sort.
        declaration :: !(KoreSortDeclaration sort symbol name)
        -- ^ Information needed for declaring the sort, also listing all
        -- dependencies on other sorts and symbols.
        }

instance
  (Show sort, Show symbol, Show name)
  => Show (Sort sort symbol name)
  where
  show s@(Sort _ _) =
    case s of
      Sort {smtFromSortArgs = _, declaration} ->
        "Sort { smtFromSortArgs, declaration = "
          ++ show declaration
          ++ "}"

{-| A representation of the Kore SymbolOrAlias type together with symbol
declaration sentences, optimized for dealing with the SMT.

The SmtSymbol type below instantiates Symbol with the types used by the SMT.
-}
data Symbol sort name
  = Symbol
      { smtFromSortArgs
          :: !(Map Kore.Id SmtSort -> [Kore.Sort] -> Maybe AST.SExpr),
        -- ^ Produces the SMT representation of a symbol. Given a map with
        -- Smt representations for sorts and a list of sort arguments, returns
        -- an s-expression that can be used, say, when building assertions
        -- using that symbol.
        declaration :: !(KoreSymbolDeclaration sort name)
        -- ^ Information needed for declaring the symbol, also listing all
        -- dependencies on other sorts and symbols.
        }

instance (Show sort, Show name) => Show (Symbol sort name) where
  show s@(Symbol _ _) =
    case s of
      Symbol {smtFromSortArgs = _, declaration} ->
        "Symbol { smtFromSortArgs, declaration = "
          ++ show declaration
          ++ "}"

{-| Data needed for declaring an SMT sort.

The SmtKoreSortDeclaration type below instantiates Sort with the types used
by the SMT.
-}
data KoreSortDeclaration sort symbol name
  = SortDeclarationDataType !(AST.DataTypeDeclaration sort symbol name)
    -- ^ Constructor-based sort. Assumed to declare its own constructors.
  | SortDeclarationSort !(AST.SortDeclaration name)
    -- ^ Non-constructor sort.
  | SortDeclaredIndirectly !name
    -- ^ Sort that we don't need to declare (e.g. builtins like Int) so we just
    -- represent that the SMT already knows about it.
  deriving (Eq, Ord, Show)

{-| Data needed for declaring an SMT symbol.

The SmtKoreSymbolDeclaration type below instantiates Symbol with the types used
by the SMT.
-}
data KoreSymbolDeclaration sort name
  = SymbolDeclaredDirectly !(AST.FunctionDeclaration sort name)
    -- ^ Normal symbol declaration
  | SymbolDeclaredIndirectly !(IndirectSymbolDeclaration sort name)
    -- ^ Symbol declared in a different way (e.g. builtin or constructor).
    -- The IndirectSymbolDeclaration value holds dependencies on other sorts
    -- and debug information.
  deriving (Eq, Ord, Show)

{-| Holds the sorts on which an already declared symbol depends.
-}
data IndirectSymbolDeclaration sort name
  = IndirectSymbolDeclaration
      { name :: !name,
        sorts :: ![sort]
        }
  deriving (Eq, Ord, Show)

{-| Holds things that we declare to an SMT. When encountered in its
SmtDeclarations instatiation, we usually assume that all dependencies between
the various declarations can be resolved.
-}
data Declarations sort symbol name
  = Declarations
      { sorts :: Map Kore.Id (Sort sort symbol name),
        symbols :: Map Kore.Id (Symbol sort name)
        }
  deriving (Show)

{-| Marks a dependency on a given sort.
-}
newtype SortReference = SortReference {getSortReference :: Kore.Sort}
  deriving (Eq, Ord, Show)

{-| Marks a dependency on a given symbol.
-}
newtype SymbolReference
  = SymbolReference {getSymbolReference :: Kore.Id}
  deriving (Eq, Ord, Show)

{-| Data that should be encoded before being used with the SMT.

Use @AlreadyEncoded@ and @encodable@ to create it, @encode@ to extract its data,
and @appendToEncoding@ to modify it.
-}
data Encodable
  = AlreadyEncoded !Text
  | Encodable !Text
  -- TODO (virgil): maybe use Id in Encodable to make it more obvious what
  -- happens.
  deriving (Eq, Ord, Show)

-- Type instantiations to be used by the SMT.
type SmtDeclarations = Declarations AST.SExpr Text Text

type SmtKoreSymbolDeclaration = KoreSymbolDeclaration AST.SExpr Text

type SmtSort = Sort AST.SExpr Text Text

type SmtSymbol = Symbol AST.SExpr Text

-- Type instantiations with unresolved dependencies, produced direclty from the
-- input module.
type UnresolvedConstructorArgument
  = AST.ConstructorArgument SortReference Encodable

type UnresolvedConstructor
  = AST.Constructor SortReference SymbolReference Encodable

type UnresolvedDataTypeDeclaration
  = AST.DataTypeDeclaration SortReference SymbolReference Encodable

type UnresolvedDeclarations
  = Declarations SortReference SymbolReference Encodable

type UnresolvedFunctionDeclaration
  = AST.FunctionDeclaration SortReference Encodable

type UnresolvedIndirectSymbolDeclaration
  = IndirectSymbolDeclaration SortReference Encodable

type UnresolvedKoreSortDeclaration
  = KoreSortDeclaration SortReference SymbolReference Encodable

type UnresolvedKoreSymbolDeclaration
  = KoreSymbolDeclaration SortReference Encodable

type UnresolvedSort
  = Sort SortReference SymbolReference Encodable

type UnresolvedSortDeclaration
  = AST.SortDeclaration Encodable

type UnresolvedSymbol
  = Symbol SortReference Encodable

encodable :: Kore.Id -> Encodable
encodable Kore.Id {getId} = Encodable getId

encode :: Encodable -> Text
encode (AlreadyEncoded e) = e
encode (Encodable e) = encodeName e

appendToEncoding :: Encodable -> Text -> Encodable
appendToEncoding (AlreadyEncoded e) t = AlreadyEncoded (e <> t)
appendToEncoding (Encodable e) t = Encodable (e <> t)

mergePreferFirst
  :: Declarations sort symbol name
  -> Declarations sort symbol name
  -> Declarations sort symbol name
mergePreferFirst
  Declarations {sorts = sorts1, symbols = symbols1}
  Declarations {sorts = sorts2, symbols = symbols2} =
    Declarations
      { sorts = sorts1 `Map.union` sorts2,
        symbols = symbols1 `Map.union` symbols2
        }
