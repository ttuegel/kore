{-|
Module      : Kore.Unification.Error
Description : Utilities for unification errors
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Unification.Error
    ( SubstitutionError (..)
    , UnificationError (..)
    , UnificationOrSubstitutionError (..)
    , substitutionToUnifyOrSubError
    , unificationToUnifyOrSubError
    , unsupportedPatterns
    ) where

import Data.Function
    ( on
    )
import Data.Text.Prettyprint.Doc
    ( Pretty
    )
import qualified Data.Text.Prettyprint.Doc as Pretty
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
import qualified Kore.Internal.Predicate as Predicate
import Kore.Internal.TermLike
    ( InternalVariable
    , TermLike
    , mapVariables
    )
import Kore.Sort
import Kore.Syntax.Application
import Kore.Syntax.Variable
import Kore.Unification.Substitution
    ( Normalization
    , wrapNormalization
    )
import Kore.Unparser
import Kore.Variables.UnifiedVariable
    ( UnifiedVariable (..)
    )

-- | Hack sum-type to wrap unification and substitution errors
data UnificationOrSubstitutionError
    = UnificationError UnificationError
    | SubstitutionError SubstitutionError
    deriving (GHC.Generic, Show)

instance SOP.Generic UnificationOrSubstitutionError

instance SOP.HasDatatypeInfo UnificationOrSubstitutionError

instance Debug UnificationOrSubstitutionError

instance Diff UnificationOrSubstitutionError

instance Pretty UnificationOrSubstitutionError where
    pretty (UnificationError  err) = Pretty.pretty err
    pretty (SubstitutionError err) = Pretty.pretty err

-- |'UnificationError' specifies various error cases encountered during
-- unification
data UnificationError = UnsupportedPatterns
    { message :: String
    , first :: TermLike Variable
    , second :: TermLike Variable
    }
    deriving (Eq, GHC.Generic, Show)

instance SOP.Generic UnificationError

instance SOP.HasDatatypeInfo UnificationError

instance Debug UnificationError

instance Diff UnificationError

unsupportedPatterns
    :: SortedVariable variable
    => String -> TermLike variable -> TermLike variable -> UnificationError
unsupportedPatterns message =
    UnsupportedPatterns message `on` mapVariables toVariable

instance Pretty UnificationError where
    pretty UnsupportedPatterns { message, first, second } =
        Pretty.vsep
            [ "Unsupported patterns: " <> Pretty.pretty message
            , "first = "
            , Pretty.indent 4 . Pretty.pretty . unparseToString $ first
            , "second = "
            , Pretty.indent 4 . Pretty.pretty . unparseToString $ second
            ]

-- |@ClashReason@ describes the head of a pattern involved in a clash.
data ClashReason
    = HeadClash SymbolOrAlias
    | DomainValueClash String
    | SortInjectionClash Sort Sort
    deriving (Eq, Show)

{-| 'SubstitutionError' specifies the various error cases related to
substitutions.
-}
data SubstitutionError =
    forall variable.
    InternalVariable variable =>
    SimplifiableCycle [UnifiedVariable variable] (Normalization variable)
    -- ^ the circularity path may pass through non-constructors: maybe solvable.

instance Debug SubstitutionError where
    debugPrec (SimplifiableCycle variables normalization) prec =
        (if prec >= 10 then Pretty.parens else id)
        $ Pretty.hsep
            [ "SimplifiableCycle"
            , debugPrec variables 10
            , debugPrec normalization 10
            ]

instance Diff SubstitutionError where
    diffPrec = diffPrecIgnore

instance Show SubstitutionError where
    showsPrec prec (SimplifiableCycle variables normalization) =
        showParen (prec >= 10)
        $ showString "SimplifiableCycle "
        . showList variables
        . showString " "
        . showsPrec 10 normalization

instance Pretty SubstitutionError where
    pretty (SimplifiableCycle variables substitution) =
        Pretty.vsep
            [ "Simplifiable circular variable dependency:"
            , (Pretty.indent 4 . Pretty.vsep) (unparse <$> variables)
            , "in substitution:"
            , (Pretty.indent 4 . unparse)
                (Predicate.fromSubstitution $ wrapNormalization substitution)
            ]

-- Trivially promote substitution errors to sum-type errors
substitutionToUnifyOrSubError
    :: SubstitutionError
    -> UnificationOrSubstitutionError
substitutionToUnifyOrSubError = SubstitutionError

-- Trivially promote unification errors to sum-type errors
unificationToUnifyOrSubError
    :: UnificationError
    -> UnificationOrSubstitutionError
unificationToUnifyOrSubError = UnificationError
