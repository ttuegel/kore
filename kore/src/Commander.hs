module Commander where

import qualified Data.Functor.Foldable as Recursive
import Data.Text.Prettyprint.Doc

import Kore.Syntax

data Expr
    = ExprList List
    | ExprAtom Atom

data List = List { listHead :: Head, listTail :: [Expr] }

flatExpr :: Expr -> Expr
flatExpr expr1 =
    case expr1 of
        ExprAtom _ -> expr1
        ExprList list1 ->
            ExprList list1 { listTail = concatMap flatTail (listTail list1) }
          where
            flatTail (ExprList list2)
              | listHead list1 == listHead list2 = listTail list2
            flatTail expr2 = [expr2]

data Head
    = HeadHead { headName :: String, headParams :: [Head] }
    | HeadVar { headVar :: String }
    deriving Eq

data Atom
    = AtomString String
    | AtomVar Var

data Var = Var { varName :: String, varSort :: Head }

parameters :: [Doc ann] -> Doc ann
parameters = braces . mconcat . punctuate (comma <> space)

children :: [Doc ann] -> [Doc ann]
children = prefixed
  where
    prefixed [ ]      = [ ]
    prefixed [x]      = ["└" <+> nest 2 x]
    prefixed (x : xs) = ("├" <+> nest 2 x) : prefixed xs

instance Pretty Head where
    pretty HeadHead { headName, headParams } =
        pretty headName <> parameters (pretty <$> headParams)
    pretty HeadVar { headVar } =
        pretty headVar

instance Pretty Var where
    pretty Var { varName, varSort } =
        pretty varName <> colon <> pretty varSort

instance Pretty Atom where
    pretty (AtomString string) = pretty string
    pretty (AtomVar var) = pretty var

instance Pretty Expr where
    pretty (ExprList exprList) = pretty exprList
    pretty (ExprAtom exprAtom) = pretty exprAtom

instance Pretty List where
    pretty List { listHead, listTail } =
        vsep (pretty listHead : children (pretty <$> listTail))

fromPattern :: Pattern Variable a -> Expr
fromPattern = Recursive.fold worker
  where
    worker (_ :< AndF And { andSort, andFirst, andSecond }) =
        ExprList List
            { listHead = sortedHead "\\and" [andSort]
            , listTail = [andFirst, andSecond]
            }
    worker (_ :< ApplicationF application) =
        ExprList List
            { listHead = symbolOrAliasHead applicationSymbolOrAlias
            , listTail = applicationChildren
            }
      where
        Application { applicationSymbolOrAlias } = application
        Application { applicationChildren } = application
    worker (_ :< BottomF Bottom { bottomSort }) =
        ExprList List
            { listHead = sortedHead "\\bottom" [bottomSort]
            , listTail = []
            }
    worker (_ :< CeilF Ceil { ceilOperandSort, ceilResultSort, ceilChild }) =
        ExprList List
            { listHead = sortedHead "\\ceil" [ceilOperandSort, ceilResultSort]
            , listTail = [ceilChild]
            }
    worker (_ :< DomainValueF DomainValue { domainValueSort, domainValueChild }) =
        ExprList List
            { listHead = sortedHead "\\dv" [domainValueSort]
            , listTail = [domainValueChild]
            }

sortedHead :: String -> [Sort] -> Head
sortedHead headName sorts =
    HeadHead { headName, headParams = sortHead <$> sorts }

sortHead :: Sort -> Head
sortHead (SortVariableSort sortVariable) =
    HeadVar (getIdForError $ getSortVariable sortVariable)
sortHead (SortActualSort sortActual) =
    HeadHead
        { headName = getIdForError sortActualName
        , headParams = sortHead <$> sortActualSorts
        }
  where
    SortActual { sortActualName, sortActualSorts } = sortActual

symbolOrAliasHead :: SymbolOrAlias -> Head
symbolOrAliasHead symbolOrAlias =
    HeadHead
        { headName = getIdForError symbolOrAliasConstructor
        , headParams = sortHead <$> symbolOrAliasParams
        }
  where
    SymbolOrAlias { symbolOrAliasConstructor } = symbolOrAlias
    SymbolOrAlias { symbolOrAliasParams } = symbolOrAlias
