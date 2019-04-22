{-|
Description : Rewrite and equality rules
Copyright   : (c) Runtime Verification, 2018
License     : NCSA

-}
module Kore.Step.Rule
    ( EqualityRule (..)
    , RewriteRule (..)
    , OnePathRule (..)
    , AllPathRule (..)
    , RulePattern (..)
    , isHeatingRule
    , isCoolingRule
    , isNormalRule
    , QualifiedAxiomPattern (..)
    , AxiomPatternError (..)
    , verifiedKoreSentenceToAxiomPattern
    , koreSentenceToAxiomPattern
    , extractRewriteAxioms
    , extractOnePathClaims
    , extractAllPathClaims
    , mkRewriteAxiom
    , mkEqualityAxiom
    , refreshRulePattern
    , freeVariables
    , Kore.Step.Rule.mapVariables
    , substitute
    ) where

import           Control.Comonad
import           Data.Map.Strict
                 ( Map )
import           Data.Maybe
import           Data.Set
                 ( Set )
import qualified Data.Set as Set
import           Data.Text.Prettyprint.Doc
                 ( Pretty )
import qualified Data.Text.Prettyprint.Doc as Pretty

import           Kore.AST.Kore hiding
                 ( freeVariables )
import           Kore.AST.Sentence
import           Kore.AST.Valid hiding
                 ( freeVariables )
import qualified Kore.AST.Valid as Valid
import qualified Kore.Attribute.Axiom as Attribute
import qualified Kore.Attribute.Parser as Attribute.Parser
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
import           Kore.Predicate.Predicate
                 ( Predicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Pattern
                 ( CommonStepPattern, StepPattern )
import qualified Kore.Step.Pattern as Pattern
import           Kore.Unparser
                 ( Unparse, unparse )
import           Kore.Variables.Fresh

newtype AxiomPatternError = AxiomPatternError ()

{- | Normal rewriting and function axioms, claims and patterns.

 -}
data RulePattern level variable = RulePattern
    { left  :: !(StepPattern level variable)
    , right :: !(StepPattern level variable)
    , requires :: !(Predicate level variable)
    , ensures :: !(Predicate level variable)
    , attributes :: !Attribute.Axiom
    }
    deriving (Eq, Ord, Show)

instance Unparse (variable level) => Pretty (RulePattern level variable) where
    pretty rulePattern@(RulePattern _ _ _ _ _) =
        Pretty.vsep
            [ "left:"
            , Pretty.indent 4 (unparse left)
            , "right:"
            , Pretty.indent 4 (unparse right)
            , "requires:"
            , Pretty.indent 4 (unparse requires)
            , "ensures:"
            , Pretty.indent 4 (unparse ensures)
            ]
      where
        RulePattern { left, right, requires, ensures } = rulePattern

{-  | Equality-based rule pattern.
-}
newtype EqualityRule level variable =
    EqualityRule { getEqualityRule :: RulePattern level variable }
    deriving (Eq, Show)

{-  | Rewrite-based rule pattern.
-}
newtype RewriteRule level variable =
    RewriteRule { getRewriteRule :: RulePattern level variable }
    deriving (Eq, Show)

instance (Unparse (variable level), Ord (variable level)) => Unparse (RewriteRule level variable) where
    unparse (RewriteRule RulePattern { left, right, requires } ) =
        unparse
            $ Valid.mkImplies
                (Valid.mkAnd left (Predicate.unwrapPredicate requires))
                right

qualifiedAxiomOpToConstructor
    :: SymbolOrAlias Object
    -> Maybe
        (RulePattern level variable -> QualifiedAxiomPattern level variable)
qualifiedAxiomOpToConstructor patternHead = case headName of
    "weakExistsFinally" -> Just $ OnePathClaimPattern . OnePathRule
    "weakAlwaysFinally" -> Just $ AllPathClaimPattern . AllPathRule
    _ -> Nothing
  where
    headName = getId (symbolOrAliasConstructor patternHead)

{-  | One-Path-Claim rule pattern.
-}
newtype OnePathRule level variable =
    OnePathRule { getOnePathRule :: RulePattern level variable }
    deriving (Eq, Show)

{-  | All-Path-Claim rule pattern.
-}
newtype AllPathRule level variable =
    AllPathRule { getAllPathRule :: RulePattern level variable }
    deriving (Eq, Show)

{- | Sum type to distinguish rewrite axioms (used for stepping)
from function axioms (used for functional simplification).
--}
data QualifiedAxiomPattern level variable
    = RewriteAxiomPattern (RewriteRule level variable)
    | FunctionAxiomPattern (EqualityRule level variable)
    | OnePathClaimPattern (OnePathRule level variable)
    | AllPathClaimPattern (AllPathRule level variable)
    -- TODO(virgil): Rename the above since it applies to all sorts of axioms,
    -- not only to function-related ones.
    deriving (Eq, Show)

{- | Does the axiom pattern represent a heating rule?
 -}
isHeatingRule :: RulePattern level variable -> Bool
isHeatingRule RulePattern { attributes } =
    case Attribute.heatCool attributes of
        Attribute.Heat -> True
        _ -> False

{- | Does the axiom pattern represent a cooling rule?
 -}
isCoolingRule :: RulePattern level variable -> Bool
isCoolingRule RulePattern { attributes } =
    case Attribute.heatCool attributes of
        Attribute.Cool -> True
        _ -> False

{- | Does the axiom pattern represent a normal rule?
 -}
isNormalRule :: RulePattern level variable -> Bool
isNormalRule RulePattern { attributes } =
    case Attribute.heatCool attributes of
        Attribute.Normal -> True
        _ -> False


-- | Extracts all 'RewriteRule' axioms matching a given @level@ from
-- a verified definition.
extractRewriteAxioms
    :: MetaOrObject level
    => level -- ^expected level for the axiom pattern
    -> VerifiedModule declAtts axiomAtts
    -- ^'IndexedModule' containing the definition
    -> [RewriteRule level Variable]
extractRewriteAxioms level idxMod =
    mapMaybe
        ( extractRewriteAxiomFrom level
        . getIndexedSentence
        )
        (indexedModuleAxioms idxMod)

extractRewriteAxiomFrom
    :: MetaOrObject level
    => level -- ^expected level for the axiom pattern
    -> SentenceAxiom UnifiedSortVariable VerifiedKorePattern
    -- ^ Sentence to extract axiom pattern from
    -> Maybe (RewriteRule level Variable)
extractRewriteAxiomFrom level sentence =
    case verifiedKoreSentenceToAxiomPattern level koreSentence of
        Right (RewriteAxiomPattern axiomPat) -> Just axiomPat
        _ -> Nothing
  where
    koreSentence = constructUnifiedSentence SentenceAxiomSentence sentence

-- | Extracts all One-Path claims from a verified module.
extractOnePathClaims
    :: VerifiedModule declAtts axiomAtts
    -- ^'IndexedModule' containing the definition
    -> [(axiomAtts, OnePathRule Object Variable)]
extractOnePathClaims idxMod =
    mapMaybe
        ( sequence                             -- (a, Maybe b) -> Maybe (a,b)
        . fmap extractOnePathClaimFrom         -- applying on second component
        )
    $ (indexedModuleClaims idxMod)

extractOnePathClaimFrom
    :: SentenceAxiom UnifiedSortVariable VerifiedKorePattern
    -- ^ Sentence to extract axiom pattern from
    -> Maybe (OnePathRule Object Variable)
extractOnePathClaimFrom sentence =
    case verifiedKoreSentenceToAxiomPattern Object koreSentence of
        Right (OnePathClaimPattern axiomPat) -> Just axiomPat
        _ -> Nothing
  where
    koreSentence = constructUnifiedSentence SentenceAxiomSentence sentence

-- | Extracts all All-Path claims from a verified definition.
extractAllPathClaims
    :: VerifiedModule declAtts axiomAtts
    -- ^'IndexedModule' containing the definition
    -> [(axiomAtts, AllPathRule Object Variable)]
extractAllPathClaims idxMod =
    mapMaybe
        ( sequence                             -- (a, Maybe b) -> Maybe (a,b)
        . fmap extractAllPathClaimFrom         -- applying on second component
        )
    $ (indexedModuleClaims idxMod)

extractAllPathClaimFrom
    :: SentenceAxiom UnifiedSortVariable VerifiedKorePattern
    -- ^ Sentence to extract axiom pattern from
    -> Maybe (AllPathRule Object Variable)
extractAllPathClaimFrom sentence =
    case verifiedKoreSentenceToAxiomPattern Object koreSentence of
        Right (AllPathClaimPattern axiomPat) -> Just axiomPat
        _ -> Nothing
  where
    koreSentence = constructUnifiedSentence SentenceAxiomSentence sentence

-- | Attempts to extract a 'QualifiedAxiomPattern' of the given @level@ from
-- a given 'KoreSentence'.
verifiedKoreSentenceToAxiomPattern
    :: MetaOrObject level
    => level
    -> VerifiedKoreSentence
    -> Either (Error AxiomPatternError) (QualifiedAxiomPattern level Variable)
verifiedKoreSentenceToAxiomPattern level =
    \case
        UnifiedObjectSentence object -> sentenceToAxiomPattern level object

-- | Attempts to extract a 'QualifiedAxiomPattern' of the given @level@ from
-- a given 'KoreSentence'.
koreSentenceToAxiomPattern
    :: MetaOrObject level
    => level
    -> VerifiedKoreSentence
    -> Either (Error AxiomPatternError) (QualifiedAxiomPattern level Variable)
koreSentenceToAxiomPattern level =
    \case
        UnifiedObjectSentence object -> sentenceToAxiomPattern level object

sentenceToAxiomPattern
    :: MetaOrObject level
    => level
    -> Sentence level' UnifiedSortVariable VerifiedKorePattern
    -> Either (Error AxiomPatternError) (QualifiedAxiomPattern level Variable)
sentenceToAxiomPattern
    level
    (SentenceAxiomSentence SentenceAxiom
        { sentenceAxiomPattern
        , sentenceAxiomAttributes
        }
    )
  = do
    attributes <-
        (Attribute.Parser.liftParser . Attribute.Parser.parseAttributes)
            sentenceAxiomAttributes
    stepPattern <- Pattern.fromKorePattern level sentenceAxiomPattern
    patternToAxiomPattern attributes stepPattern
sentenceToAxiomPattern _ _ =
    koreFail "Only axiom sentences can be translated to AxiomPatterns"

{- | Match a pure pattern encoding an 'QualifiedAxiomPattern'.

@patternToAxiomPattern@ returns an error if the given 'CommonPurePattern' does
not encode a normal rewrite or function axiom.
-}
patternToAxiomPattern
    :: MetaOrObject level
    => Attribute.Axiom
    -> CommonStepPattern level
    -> Either (Error AxiomPatternError) (QualifiedAxiomPattern level Variable)
patternToAxiomPattern attributes pat =
    case pat of
        -- normal rewrite axioms
        -- TODO (thomas.tuegel): Allow \and{_}(ensures, rhs) to be wrapped in
        -- quantifiers.
        Rewrites_ _ (And_ _ requires lhs) (And_ _ ensures rhs) ->
            pure $ RewriteAxiomPattern $ RewriteRule RulePattern
                { left = lhs
                , right = rhs
                , requires = Predicate.wrapPredicate requires
                , ensures = Predicate.wrapPredicate ensures
                , attributes
                }
        -- Reachability claims
        Implies_ _ (And_ _ requires lhs) (App_ op [And_ _ ensures rhs])
          | Just constructor <- qualifiedAxiomOpToConstructor op ->
            pure $ constructor RulePattern
                { left = lhs
                , right = rhs
                , requires = Predicate.wrapPredicate requires
                , ensures = Predicate.wrapPredicate ensures
                , attributes
                }
        -- function axioms: general
        Implies_ _ requires (And_ _ (Equals_ _ _ lhs rhs) _ensures) ->
            -- TODO (traiansf): ensure that _ensures is \top
            pure $ FunctionAxiomPattern $ EqualityRule RulePattern
                { left = lhs
                , right = rhs
                , requires = Predicate.wrapPredicate requires
                , ensures = Predicate.makeTruePredicate
                , attributes
                }
        -- function axioms: trivial pre- and post-conditions
        Equals_ _ _ lhs rhs ->
            pure $ FunctionAxiomPattern $ EqualityRule RulePattern
                { left = lhs
                , right = rhs
                , requires = Predicate.makeTruePredicate
                , ensures = Predicate.makeTruePredicate
                , attributes
                }
        Forall_ _ _ child -> patternToAxiomPattern attributes child
        _ -> koreFail "Unsupported pattern type in axiom"

{- | Construct a 'VerifiedKoreSentence' corresponding to 'RewriteRule'.

The requires clause must be a predicate, i.e. it can occur in any sort.

 -}
mkRewriteAxiom
    :: CommonStepPattern Object  -- ^ left-hand side
    -> CommonStepPattern Object  -- ^ right-hand side
    -> Maybe (Sort Object -> CommonStepPattern Object)  -- ^ requires clause
    -> VerifiedKoreSentence
mkRewriteAxiom lhs rhs requires =
    (asKoreAxiomSentence . Pattern.toKoreSentenceAxiom . mkAxiom_)
        (mkRewrites
            (mkAnd (fromMaybe mkTop requires $ patternSort) lhs)
            (mkAnd (mkTop patternSort) rhs)
        )
  where
    Valid { patternSort } = extract lhs

{- | Construct a 'VerifiedKoreSentence' corresponding to 'EqualityRule'.

The requires clause must be a predicate, i.e. it can occur in any sort.

 -}
mkEqualityAxiom
    :: CommonStepPattern Object  -- ^ left-hand side
    -> CommonStepPattern Object  -- ^ right-hand side
    -> Maybe (Sort Object -> CommonStepPattern Object)  -- ^ requires clause
    -> VerifiedKoreSentence
mkEqualityAxiom lhs rhs requires =
    asKoreAxiomSentence
    $ Pattern.toKoreSentenceAxiom
    $ mkAxiom [sortVariableR]
    $ case requires of
        Just requires' ->
            mkImplies (requires' sortR) (mkAnd function mkTop_)
        Nothing -> function
  where
    sortVariableR = SortVariable "R"
    sortR = SortVariableSort sortVariableR
    function = mkEquals sortR lhs rhs

{- | Refresh the variables of a 'RulePattern'.

The free variables of a 'RulePattern' are implicitly quantified, so are renamed
to avoid collision with any variables in the given set.

 -}
refreshRulePattern
    :: forall variable level
    .   ( FreshVariable variable
        , SortedVariable variable
        , Ord (variable level)
        , MetaOrObject level
        )
    => Set (variable level)  -- ^ Variables to avoid
    -> RulePattern level variable
    -> (Map (variable level) (variable level), RulePattern level variable)
refreshRulePattern avoid rulePattern =
    let rename = refreshVariables avoid originalFreeVariables
        subst = mkVar <$> rename
        left' = Pattern.substitute subst left
        right' = Pattern.substitute subst right
        requires' = Predicate.substitute subst requires
        rulePattern' =
            rulePattern
                { left = left'
                , right = right'
                , requires = requires'
                }
    in (rename, rulePattern')
  where
    RulePattern { left, right, requires } = rulePattern
    originalFreeVariables = freeVariables rulePattern

{- | Extract the free variables of a 'RulePattern'.
 -}
freeVariables
    ::  ( MetaOrObject level
        , Ord (variable level)
        )
    => RulePattern level variable
    -> Set (variable level)
freeVariables RulePattern { left, right, requires } =
    Set.unions
        [ (Valid.freeVariables . extract) left
        , (Valid.freeVariables . extract) right
        , Predicate.freeVariables requires
        ]

{- | Apply the given function to all variables in a 'RulePattern'.
 -}
mapVariables
    :: Ord (variable2 level)
    => (variable1 level -> variable2 level)
    -> RulePattern level variable1
    -> RulePattern level variable2
mapVariables mapping rulePattern =
    rulePattern
        { left = Pattern.mapVariables mapping left
        , right = Pattern.mapVariables mapping right
        , requires = Predicate.mapVariables mapping requires
        , ensures = Predicate.mapVariables mapping ensures
        }
  where
    RulePattern { left, right, requires, ensures } = rulePattern


{- | Traverse the predicate from the top down and apply substitutions.

The 'freeVariables' annotation is used to avoid traversing subterms that
contain none of the targeted variables.

 -}
substitute
    ::  ( FreshVariable variable
        , MetaOrObject level
        , Ord (variable level)
        , SortedVariable variable
        )
    => Map (variable level) (StepPattern level variable)
    -> RulePattern level variable
    -> RulePattern level variable
substitute subst rulePattern =
    rulePattern
        { left = Pattern.substitute subst left
        , right = Pattern.substitute subst right
        , requires = Predicate.substitute subst requires
        , ensures = Predicate.substitute subst ensures
        }
  where
    RulePattern { left, right, requires, ensures } = rulePattern
