{- |
Module      : Kore.Builtin
Description : Built-in sorts and symbols
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified.

@
    import qualified Kore.Builtin as Builtin
@
 -}
module Kore.Builtin
    ( Builtin.Verifiers (..)
    , Builtin.DomainValueVerifiers
    , Builtin.Function
    , Builtin
    , Builtin.sortDeclVerifier
    , Builtin.SymbolVerifier (..)
    , Builtin.symbolVerifier
    , Builtin.verifyDomainValue
    , koreVerifiers
    , koreEvaluators
    , evaluators
    , externalizePattern
    ) where

import qualified Data.Functor.Foldable as Recursive
import qualified Data.HashMap.Strict as HashMap
import           Data.Map
                 ( Map )
import qualified Data.Map as Map
import           Data.Semigroup
                 ( (<>) )
import           Data.Text
                 ( Text )

import           Kore.AST.Pure
import qualified Kore.Attribute.Axiom as Attribute
import           Kore.Attribute.Hook
                 ( Hook (..) )
import           Kore.Attribute.Symbol
                 ( StepperAttributes )
import qualified Kore.Attribute.Symbol as Attribute
import qualified Kore.Builtin.Bool as Bool
import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.KEqual as KEqual
import qualified Kore.Builtin.Krypto as Krypto
import qualified Kore.Builtin.List as List
import qualified Kore.Builtin.Map as Map
import qualified Kore.Builtin.Set as Set
import qualified Kore.Builtin.String as String
import qualified Kore.Domain.Builtin as Domain
import           Kore.IndexedModule.IndexedModule
                 ( IndexedModule (..), VerifiedModule )
import qualified Kore.IndexedModule.IndexedModule as IndexedModule
import qualified Kore.Parser.Pattern as Parser
import           Kore.Step.Axiom.Identifier
                 ( AxiomIdentifier )
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
                 ( AxiomIdentifier (..) )
import           Kore.Step.TermLike

{- | The default type of builtin domain values.
 -}
type Builtin = DomainValue Sort (Domain.Builtin (TermLike Variable))

{- | Verifiers for Kore builtin sorts.

  If you aren't sure which verifiers you need, use these.

 -}
koreVerifiers :: Builtin.Verifiers
koreVerifiers =
    Builtin.Verifiers
    { sortDeclVerifiers =
           Bool.sortDeclVerifiers
        <> Int.sortDeclVerifiers
        <> List.sortDeclVerifiers
        <> Map.sortDeclVerifiers
        <> Set.sortDeclVerifiers
        <> String.sortDeclVerifiers
    , symbolVerifiers =
           Bool.symbolVerifiers
        <> Int.symbolVerifiers
        <> List.symbolVerifiers
        <> Map.symbolVerifiers
        <> KEqual.symbolVerifiers
        <> Set.symbolVerifiers
        <> String.symbolVerifiers
        <> Krypto.symbolVerifiers
    , domainValueVerifiers =
        HashMap.fromList
            [ (Bool.sort, Bool.patternVerifier)
            , (Int.sort, Int.patternVerifier)
            , (String.sort, String.patternVerifier)
            ]
    }

{- | Construct an evaluation context for Kore builtin functions.

  Returns a map from symbol identifiers to builtin functions used for function
  evaluation in the context of the given module.

  See also: 'Data.Step.Step.step'

 -}
koreEvaluators
    :: VerifiedModule StepperAttributes Attribute.Axiom
    -- ^ Module under which evaluation takes place
    -> Map (AxiomIdentifier Object) Builtin.Function
koreEvaluators = evaluators builtins
  where
    builtins :: Map Text Builtin.Function
    builtins =
        Map.unions
            [ Bool.builtinFunctions
            , Int.builtinFunctions
            , KEqual.builtinFunctions
            , List.builtinFunctions
            , Map.builtinFunctions
            , Set.builtinFunctions
            , String.builtinFunctions
            , Krypto.builtinFunctions
            ]

{- | Construct an evaluation context for the given builtin functions.

  Returns a map from symbol identifiers to builtin functions used for function
  evaluation in the context of the given module.

  See also: 'Data.Step.Step.step', 'koreEvaluators'

 -}
evaluators
    :: Map Text Builtin.Function
    -- ^ Builtin functions indexed by name
    -> VerifiedModule StepperAttributes Attribute.Axiom
    -- ^ Module under which evaluation takes place
    -> Map (AxiomIdentifier Object) Builtin.Function
evaluators builtins indexedModule =
    Map.mapMaybe
        lookupBuiltins
        (Map.mapKeys
            AxiomIdentifier.Application
            (hookedSymbolAttributes indexedModule)
        )
  where
    hookedSymbolAttributes
        :: VerifiedModule StepperAttributes Attribute.Axiom
        -> Map Id StepperAttributes
    hookedSymbolAttributes im =
        Map.union
            (justAttributes <$> IndexedModule.hookedObjectSymbolSentences im)
            (Map.unions
                (importHookedSymbolAttributes <$> indexedModuleImports im)
            )
      where
        justAttributes (attrs, _) = attrs

    importHookedSymbolAttributes
        :: (a, b, VerifiedModule StepperAttributes Attribute.Axiom)
        -> Map Id StepperAttributes
    importHookedSymbolAttributes (_, _, im) = hookedSymbolAttributes im

    lookupBuiltins :: StepperAttributes -> Maybe Builtin.Function
    lookupBuiltins Attribute.Symbol { Attribute.hook = Hook { getHook } } =
        do
            name <- getHook
            impl <- Map.lookup name builtins
            pure impl

{- | Externalize all builtin domain values in the given pattern.

All builtins will be rendered using their concrete Kore syntax.

See also: 'asPattern'

 -}
-- TODO (thomas.tuegel): Transform from Domain.Internal to Domain.External.
externalizePattern
    ::  forall variable. Ord variable
    =>  TermLike variable
    ->  Parser.Pattern variable
externalizePattern =
    Recursive.unfold externalizePatternWorker
  where
    externalizePatternWorker
        ::  TermLike variable
        ->  Base (Parser.Pattern variable) (TermLike variable)
    externalizePatternWorker (Recursive.project -> _ :< pat) =
        case pat of
            DomainValuePattern domain ->
                case domain of
                    Domain.BuiltinExternal external ->
                        (Parser.asPatternBase . Parser.DomainValueF)
                            DomainValue { domainValueSort, domainValueChild }
                      where
                        Domain.External { domainValueSort } = external
                        Domain.External { domainValueChild } = external
                    Domain.BuiltinMap  builtin ->
                        externalizePatternWorker (Map.asTermLike builtin)
                    Domain.BuiltinList builtin ->
                        externalizePatternWorker (List.asTermLike builtin)
                    Domain.BuiltinSet  builtin ->
                        externalizePatternWorker (Set.asTermLike builtin)
                    Domain.BuiltinInt  builtin ->
                        externalizePatternWorker (Int.asTermLike builtin)
                    Domain.BuiltinBool builtin ->
                        externalizePatternWorker (Bool.asTermLike builtin)
            -- Trivial cases
            InhabitantPattern s       -> Parser.asPatternBase $ Parser.InhabitantF s
            AndPattern andP           -> Parser.asPatternBase $ Parser.AndF andP
            ApplicationPattern appP   -> Parser.asPatternBase $ Parser.ApplicationF appP
            BottomPattern botP        -> Parser.asPatternBase $ Parser.BottomF botP
            CeilPattern ceilP         -> Parser.asPatternBase $ Parser.CeilF ceilP
            EqualsPattern eqP         -> Parser.asPatternBase $ Parser.EqualsF eqP
            ExistsPattern existsP     -> Parser.asPatternBase $ Parser.ExistsF existsP
            FloorPattern flrP         -> Parser.asPatternBase $ Parser.FloorF flrP
            ForallPattern forallP     -> Parser.asPatternBase $ Parser.ForallF forallP
            IffPattern iffP           -> Parser.asPatternBase $ Parser.IffF iffP
            ImpliesPattern impP       -> Parser.asPatternBase $ Parser.ImpliesF impP
            InPattern inP             -> Parser.asPatternBase $ Parser.InF inP
            NextPattern nextP         -> Parser.asPatternBase $ Parser.NextF nextP
            NotPattern notP           -> Parser.asPatternBase $ Parser.NotF notP
            OrPattern orP             -> Parser.asPatternBase $ Parser.OrF orP
            RewritesPattern rewP      -> Parser.asPatternBase $ Parser.RewritesF rewP
            StringLiteralPattern strP -> Parser.asPatternBase $ Parser.StringLiteralF strP
            CharLiteralPattern charP  -> Parser.asPatternBase $ Parser.CharLiteralF charP
            TopPattern topP           -> Parser.asPatternBase $ Parser.TopF topP
            VariablePattern varP      -> Parser.asPatternBase $ Parser.VariableF varP
            SetVariablePattern varP   -> Parser.asPatternBase $ Parser.SetVariableF varP
