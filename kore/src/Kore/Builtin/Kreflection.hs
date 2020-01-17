{- |
Module      : Kore.Builtin.Kreflection
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

-}

module Kore.Builtin.Kreflection
    ( verifiers
    ) where

import qualified Data.HashMap.Strict as HashMap
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Error

verifiers :: Builtin.Verifiers
verifiers =
    Builtin.Verifiers
        { sortDeclVerifiers = mempty
        , symbolVerifiers
        , patternVerifierHook = mempty
        }

symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ ( "KREFLECTION.isConcrete" , rejectSymbolIsConcrete)
    ]

rejectSymbolIsConcrete :: Builtin.SymbolVerifier
rejectSymbolIsConcrete =
    Builtin.SymbolVerifier $ \_ _ ->
        Kore.Error.koreFail "found KREFLECTION.isConcrete hook"
