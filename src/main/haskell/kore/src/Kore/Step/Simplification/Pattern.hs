{-|
Module      : Kore.Step.Simplification.Pattern
Description : Tools for Pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Pattern
    ( simplify
    , simplifyToOr
    ) where

import qualified Data.Map as Map
import           Data.Reflection
                 ( Given, give )

import           Kore.AST.Common
                 ( Id, Pattern (..), PureMLPattern, SortedVariable )
import           Kore.AST.MetaOrObject
import           Kore.AST.PureML
                 ( fromPurePattern )
import           Kore.IndexedModule.MetadataTools
                 ( MetadataTools, SymbolOrAliasSorts )
import qualified Kore.IndexedModule.MetadataTools as MetadataTools
                 ( MetadataTools (..) )
import           Kore.Step.ExpandedPattern
                 ( ExpandedPattern )
import           Kore.Step.Function.Data
                 ( ApplicationFunctionEvaluator )
import           Kore.Step.OrOfExpandedPattern
                 ( OrOfExpandedPattern )
import qualified Kore.Step.OrOfExpandedPattern as OrOfExpandedPattern
                 ( toExpandedPattern )
import qualified Kore.Step.Simplification.And as And
                 ( simplify )
import qualified Kore.Step.Simplification.Application as Application
                 ( simplify )
import qualified Kore.Step.Simplification.Bottom as Bottom
                 ( simplify )
import qualified Kore.Step.Simplification.Ceil as Ceil
                 ( simplify )
import qualified Kore.Step.Simplification.CharLiteral as CharLiteral
                 ( simplify )
import           Kore.Step.Simplification.Data
                 ( PureMLPatternSimplifier (..), SimplificationProof (..),
                 Simplifier )
import qualified Kore.Step.Simplification.DomainValue as DomainValue
                 ( simplify )
import qualified Kore.Step.Simplification.Equals as Equals
                 ( simplify )
import qualified Kore.Step.Simplification.Exists as Exists
                 ( simplify )
import qualified Kore.Step.Simplification.Floor as Floor
                 ( simplify )
import qualified Kore.Step.Simplification.Forall as Forall
                 ( simplify )
import qualified Kore.Step.Simplification.Iff as Iff
                 ( simplify )
import qualified Kore.Step.Simplification.Implies as Implies
                 ( simplify )
import qualified Kore.Step.Simplification.In as In
                 ( simplify )
import qualified Kore.Step.Simplification.Next as Next
                 ( simplify )
import qualified Kore.Step.Simplification.Not as Not
                 ( simplify )
import qualified Kore.Step.Simplification.Or as Or
                 ( simplify )
import qualified Kore.Step.Simplification.Rewrites as Rewrites
                 ( simplify )
import qualified Kore.Step.Simplification.StringLiteral as StringLiteral
                 ( simplify )
import qualified Kore.Step.Simplification.Top as Top
                 ( simplify )
import qualified Kore.Step.Simplification.Variable as Variable
                 ( simplify )
import           Kore.Step.StepperAttributes
                 ( StepperAttributes (..) )
import           Kore.Substitution.Class
                 ( Hashable )
import           Kore.Variables.Fresh

-- TODO(virgil): Add a Simplifiable class and make all pattern types
-- instances of that.

{-|'simplify' simplifies a PureMLPattern level variable, returning an
'ExpandedPattern'.
-}
simplify
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , Show (variable Meta)
        , Show (variable Object)
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level variable]
    -- ^ Map from symbol IDs to defined functions
    -> PureMLPattern level variable
    -> Simplifier
        ( ExpandedPattern level variable
        , SimplificationProof level
        )
simplify tools symbolIdToEvaluator patt = do
    (orPatt, proof) <- simplifyToOr tools symbolIdToEvaluator patt
    return
        ( give (MetadataTools.symbolOrAliasSorts tools)
            $ OrOfExpandedPattern.toExpandedPattern orPatt
        , proof
        )

{-|'simplifyToOr' simplifies a PureMLPattern level variable, returning an
'OrOfExpandedPattern'.
-}
simplifyToOr
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Show (variable level)
        , Ord (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , Show (variable Meta)
        , Show (variable Object)
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level variable]
    -- ^ Map from symbol IDs to defined functions
    -> PureMLPattern level variable
    -> Simplifier
        ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplifyToOr tools symbolIdToEvaluator patt =
    give
        (MetadataTools.symbolOrAliasSorts tools)
        (simplifyInternal
            tools
            simplifier
            symbolIdToEvaluator
            (fromPurePattern patt)
        )
  where
    simplifier = PureMLPatternSimplifier
        (simplifyToOr tools symbolIdToEvaluator)

simplifyInternal
    ::  ( MetaOrObject level
        , SortedVariable variable
        , Given (SymbolOrAliasSorts level)
        , Show (variable level)
        , Ord (variable level)
        , Ord (variable Meta)
        , Ord (variable Object)
        , Show (variable Meta)
        , Show (variable Object)
        , FreshVariable variable
        , Hashable variable
        )
    => MetadataTools level StepperAttributes
    -> PureMLPatternSimplifier level variable
    -> Map.Map (Id level) [ApplicationFunctionEvaluator level variable]
    -- ^ Map from symbol IDs to defined functions
    -> Pattern level variable (PureMLPattern level variable)
    -> Simplifier
        ( OrOfExpandedPattern level variable
        , SimplificationProof level
        )
simplifyInternal
    tools
    simplifier@(PureMLPatternSimplifier unwrappedSimplifier)
    symbolIdToEvaluator
    patt
  = do
    halfSimplified <- traverse unwrappedSimplifier patt
    -- TODO: Remove fst
    case fmap fst halfSimplified of
        AndPattern p -> And.simplify tools p
        ApplicationPattern p ->
            --  TODO: Re-evaluate outside of the application and stop passing
            -- the simplifier.
            Application.simplify
                tools
                simplifier
                symbolIdToEvaluator
                p
        BottomPattern p -> return $ Bottom.simplify p
        CeilPattern p -> return $ Ceil.simplify tools p
        DomainValuePattern p -> return $ DomainValue.simplify p
        EqualsPattern p -> Equals.simplify tools p
        ExistsPattern p -> Exists.simplify tools simplifier p
        FloorPattern p -> return $ Floor.simplify p
        ForallPattern p -> return $ Forall.simplify p
        IffPattern p -> return $ Iff.simplify p
        ImpliesPattern p -> return $ Implies.simplify p
        InPattern p -> return $ In.simplify tools p
        -- TODO(virgil): Move next up through patterns.
        NextPattern p -> return $ Next.simplify p
        NotPattern p -> return $ Not.simplify p
        OrPattern p -> return $ Or.simplify p
        RewritesPattern p -> return $ Rewrites.simplify p
        StringLiteralPattern p -> return $ StringLiteral.simplify p
        CharLiteralPattern p -> return $ CharLiteral.simplify p
        TopPattern p -> return $ Top.simplify p
        VariablePattern p -> return $ Variable.simplify p
