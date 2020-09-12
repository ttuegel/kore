module Test.Kore.Strategies.AllPath.AllPath
    ( test_unprovenNodes
    , test_transitionRule_Begin
    , test_transitionRule_CheckImplication
    , test_transitionRule_ApplyClaims
    , test_transitionRule_ApplyAxioms
    , test_runStrategy
    ) where

import Prelude.Kore

import Test.Tasty

import Control.Monad.Catch
    ( MonadCatch (catch)
    , MonadThrow (throwM)
    )
import qualified Data.Foldable as Foldable
import Data.Functor.Identity
import qualified Data.Graph.Inductive as Gr
import Data.Limit
    ( Limit (..)
    )
import Data.Sequence
    ( Seq
    )
import qualified Data.Sequence as Seq
import qualified Generics.SOP as SOP
import qualified GHC.Generics as GHC

import Kore.Debug
import qualified Kore.Internal.MultiOr as MultiOr
import Kore.Reachability.Claim
    ( AppliedRule (..)
    , Claim (..)
    )
import qualified Kore.Reachability.Claim as Claim
import Kore.Step.Simplification.Data
    ( MonadSimplify (..)
    )
import qualified Kore.Step.Strategy as Strategy
import Kore.Step.Transition
    ( runTransitionT
    )
import qualified Kore.Step.Transition as Transition
import qualified Kore.Reachability.ClaimState as ClaimState
import qualified Kore.Reachability.Prim as Prim
import Log
    ( MonadLog (..)
    )
import Pretty
    ( Pretty (..)
    )
import SMT
    ( MonadSMT (..)
    )

import Test.Terse

--
-- -- * Tests
--
test_unprovenNodes :: [TestTree]
test_unprovenNodes =
    [ unprovenNodes
        (emptyExecutionGraph ClaimState.Proven)
        `satisfies_`
        Foldable.null
    , unprovenNodes
        (goal 0)
        `satisfies_`
        (not . Foldable.null)
    , unprovenNodes
        (goal 0)
        `equals`
        MultiOr.MultiOr [0]
        $  "returns single unproven node"
    , unprovenNodes
        (goal 0
            & insNode (1, ClaimState.Claimed 1)
            & insNode (2, ClaimState.Proven)
        )
        `equals_`
        MultiOr.MultiOr [0, 1]
    , unprovenNodes
        (goal 0
            & subgoal 0 (1, ClaimState.Claimed 1)
            & subgoal 0 (2, ClaimState.Proven)
        )
        `equals_`
        MultiOr.MultiOr [1]
    , unprovenNodes
        (goal 0
            & subgoal 0 (1, ClaimState.Claimed 1)
            & subgoal 1 (2, ClaimState.Claimed 2)
            & subgoal 2 (3, ClaimState.Proven)
        )
        `equals_`
        MultiOr.MultiOr []
    , unprovenNodes
        (goal 0
            & subgoal 0 (1, ClaimState.Remaining 1)
            & subgoal 0 (2, ClaimState.Proven)
        )
        `equals_`
        MultiOr.MultiOr [1]
    ]
  where
    goal :: Integer -> ExecutionGraph
    goal n = emptyExecutionGraph (ClaimState.Claimed n)

    subgoal
        :: Gr.Node
        -> (Gr.Node, ClaimState.ClaimState Integer)
        -> ExecutionGraph -> ExecutionGraph
    subgoal parent node@(child, _) =
        insEdge (parent, child) . insNode node

test_transitionRule_Begin :: [TestTree]
test_transitionRule_Begin =
    [ done ClaimState.Proven
    , unmodified (ClaimState.Claimed    (A, B))
    , unmodified (ClaimState.Remaining (A, B))
    ]
  where
    run = runTransitionRule [] [] Prim.Begin
    unmodified :: HasCallStack => ClaimState -> TestTree
    unmodified state = run state `equals_` [(state, mempty)]
    done :: HasCallStack => ClaimState -> TestTree
    done state = run state `satisfies_` Foldable.null

test_transitionRule_CheckImplication :: [TestTree]
test_transitionRule_CheckImplication =
    [ unmodified ClaimState.Proven
    , unmodified (ClaimState.Stuck (A, B))
    , ClaimState.Claimed (B, B) `becomes` (ClaimState.Proven, mempty)
    ]
  where
    run = runTransitionRule [] [] Prim.CheckImplication
    unmodified :: HasCallStack => ClaimState -> TestTree
    unmodified state = run state `equals_` [(state, mempty)]
    becomes initial final = run initial `equals_` [final]

test_transitionRule_ApplyClaims :: [TestTree]
test_transitionRule_ApplyClaims =
    [ unmodified ClaimState.Proven
    , unmodified (ClaimState.Rewritten    (A, B))
    , [Rule (A, C)]
        `derives`
        [ (ClaimState.Rewritten (C,   C), Seq.singleton $ AppliedClaim (A, C))
        , (ClaimState.Remaining (Bot, C), mempty)
        ]
    , fmap Rule [(A, B), (B, C)]
        `derives`
        [ (ClaimState.Rewritten (B  , C), Seq.singleton $ AppliedClaim (A, B))
        , (ClaimState.Remaining (Bot, C), mempty)
        ]
    ]
  where
    run rules = runTransitionRule (map unRule rules) [] Prim.ApplyClaims
    unmodified :: HasCallStack => ClaimState -> TestTree
    unmodified state = run [Rule (A, B)] state `equals_` [(state, mempty)]
    derives
        :: HasCallStack
        => [Claim.Rule Goal]
        -- ^ rules to apply in parallel
        -> [(ClaimState, Seq (Claim.AppliedRule Goal))]
        -- ^ transitions
        -> TestTree
    derives rules = equals_ (run rules $ ClaimState.Claimed (A, C))

test_transitionRule_ApplyAxioms :: [TestTree]
test_transitionRule_ApplyAxioms =
    [ unmodified ClaimState.Proven
    , unmodified (ClaimState.Rewritten    (A, B))
    , [Rule (A, C)]
        `derives`
        [ (ClaimState.Rewritten (C,   C), Seq.singleton $ axiom (A, C))
        , (ClaimState.Remaining (Bot, C), mempty)
        ]
    , fmap Rule [(A, B), (B, C)]
        `derives`
        [ (ClaimState.Rewritten (B  , C), Seq.singleton $ axiom (A, B))
        , (ClaimState.Remaining (Bot, C), mempty)
        ]
    ]
  where
    run rules = runTransitionRule [] [rules] Prim.ApplyAxioms
    axiom = AppliedAxiom . Rule
    unmodified :: HasCallStack => ClaimState -> TestTree
    unmodified state = run [Rule (A, B)] state `equals_` [(state, mempty)]
    derives
        :: HasCallStack
        => [Claim.Rule Goal]
        -- ^ rules to apply in parallel
        -> [(ClaimState, Seq (Claim.AppliedRule Goal))]
        -- ^ transitions
        -> TestTree
    derives rules = equals_ (run rules $ ClaimState.Remaining (A, C))

test_runStrategy :: [TestTree]
test_runStrategy =
    [ [] `proves`    (A, A)
    , [] `disproves` (A, B) $ [(A, B)]

    , [Rule (A, Bot)] `proves` (A, A)
    , [Rule (A, Bot)] `proves` (A, B)

    , [Rule (A, B)] `proves`    (A, B   )
    , [Rule (A, B)] `proves`    (A, BorC)
    , [Rule (A, B)] `disproves` (A, C   ) $ [(B, C)]

    , [Rule (A, A)] `proves` (A, B)
    , [Rule (A, A)] `proves` (A, C)

    , [Rule (A, NotDef)] `disproves` (A, C) $ []

    , fmap Rule [(A, B), (A, C)] `proves`    (A, BorC)
    , fmap Rule [(A, B), (A, C)] `disproves` (A, B   ) $ [(C, B)]

    , differentLengthPaths `proves` (A, F)
    ]
  where
    run
        :: [Claim.Rule Goal]
        -> Claim.Rule Goal
        -> Strategy.ExecutionGraph ClaimState (Claim.AppliedRule Goal)
    run axioms goal =
        runIdentity
        . unAllPathIdentity
        $ Strategy.runStrategy
            Unlimited
            (Claim.transitionRule [unRule goal] [axioms])
            (Foldable.toList Claim.strategy)
            (ClaimState.Claimed . unRule $ goal)
    disproves
        :: HasCallStack
        => [Claim.Rule Goal]
        -- ^ Axioms
        -> Goal
        -- ^ Proof goal
        -> [Goal]
        -- ^ Unproven goals
        -> TestTree
    disproves axioms goal unproven =
        equals
            (Foldable.toList $ unprovenNodes $ run axioms (Rule goal))
            unproven
            (show axioms ++ " disproves " ++ show goal)
    proves
        :: HasCallStack
        => [Claim.Rule Goal]
        -- ^ Axioms
        -> Goal
        -- ^ Proof goal
        -> TestTree
    proves axioms goal =
        satisfies
            (run axioms (Rule goal))
            proven
            (show axioms ++ " proves " ++ show goal)

-- * Definitions

type ExecutionGraph = Strategy.ExecutionGraph (ClaimState.ClaimState Integer) (AppliedRule Goal)

emptyExecutionGraph :: ClaimState.ClaimState Integer -> ExecutionGraph
emptyExecutionGraph = Strategy.emptyExecutionGraph

insNode
    :: (Gr.Node, ClaimState.ClaimState Integer)
    -> ExecutionGraph
    -> ExecutionGraph
insNode = Strategy.insNode

insEdge
    :: (Gr.Node, Gr.Node)
    -> ExecutionGraph
    -> ExecutionGraph
insEdge = Strategy.insEdge

-- | Simple program configurations for unit testing.
data K = BorC | A | B | C | D | E | F | NotDef | Bot
    deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic K

instance SOP.HasDatatypeInfo K

instance Debug K

instance Diff K

instance Pretty K where
    pretty = pretty . show

matches :: K -> K -> Bool
matches B BorC = True
matches C BorC = True
matches a b    = a == b

difference :: K -> K -> K
difference BorC B = C
difference BorC C = B
difference a    b
  | a `matches` b = Bot
  | otherwise     = a

type Goal = (K, K)

type ClaimState = ClaimState.ClaimState Goal

type Prim = Claim.Prim

instance Claim Goal where

    newtype instance Rule Goal = Rule { unRule :: (K, K) }
        deriving (Eq, GHC.Generic, Show)

    checkImplication (src, dst)
      | src' == Bot = return Claim.Implied
      | src == NotDef = return Claim.Implied
      | otherwise = return $ Claim.NotImplied (src', dst)
      where
        src' = difference src dst

    simplify = return

    applyClaims claims =
        derivePar AppliedClaim (map Rule claims)

    applyAxioms axiomGroups =
        derivePar (AppliedAxiom . Rule) (concat axiomGroups)

derivePar
    :: (Goal -> Claim.AppliedRule Goal)
    -> [Claim.Rule Goal]
    -> (K, K)
    -> Transition.TransitionT (Claim.AppliedRule Goal) m (ClaimState.ClaimState (K, K))
derivePar mkAppliedRule rules (src, dst) =
    goals <|> goalRemainder
  where
    goal (Rule rule@(_, to)) = do
        Transition.addRule (mkAppliedRule rule)
        (pure . ClaimState.Rewritten) (to, dst)
    goalRemainder = do
        let r = Foldable.foldl' difference src (fst . unRule <$> applied)
        (pure . ClaimState.Remaining) (r, dst)
    applyRule rule@(Rule (fromGoal, _))
        | fromGoal `matches` src = Just rule
        | otherwise = Nothing
    applied = mapMaybe applyRule rules
    goals = Foldable.asum (goal <$> applied)

instance SOP.Generic (Claim.Rule Goal)

instance SOP.HasDatatypeInfo (Claim.Rule Goal)

instance Debug (Claim.Rule Goal)

instance Diff (Claim.Rule Goal)

runTransitionRule
    :: [Goal]
    -> [[Claim.Rule Goal]]
    -> Prim
    -> ClaimState
    -> [(ClaimState, Seq (Claim.AppliedRule Goal))]
runTransitionRule claims axiomGroups prim state =
    (runIdentity . unAllPathIdentity . runTransitionT)
        (Claim.transitionRule claims axiomGroups prim state)

newtype AllPathIdentity a = AllPathIdentity { unAllPathIdentity :: Identity a }
    deriving (Functor, Applicative, Monad)

instance MonadLog AllPathIdentity where
    logEntry = undefined
    logWhile _ = undefined

instance MonadSMT AllPathIdentity where
    withSolver = undefined
    declare = undefined
    declareFun = undefined
    declareSort = undefined
    declareDatatype = undefined
    declareDatatypes = undefined
    assert = undefined
    check = undefined
    ackCommand = undefined
    loadFile = undefined
    reinit = undefined

instance MonadThrow AllPathIdentity where
    throwM _ = error "Unimplemented"

instance MonadCatch AllPathIdentity where
    catch action _handler = action

instance MonadSimplify AllPathIdentity where
    askMetadataTools = undefined
    simplifyTermLike = undefined
    simplifyCondition = undefined
    askSimplifierAxioms = undefined
    localSimplifierAxioms = undefined
    askMemo = undefined
    askInjSimplifier = undefined
    askOverloadSimplifier = undefined

differentLengthPaths :: [Claim.Rule Goal]
differentLengthPaths =
    fmap Rule
    [ -- Length 5 path
      (A, B), (B, C), (C, D), (D, E), (E, F)
      -- Length 4 path
    ,                         (D, F)
      -- Length 3 path
    ,                 (C, F)
      -- Length 2 path
    ,         (B, F)
      -- Length 1 path
    , (A, F)
    ]

{- | The final nodes of an execution graph which were not proven.

See also: 'Strategy.pickFinal', 'extractUnproven'

 -}
unprovenNodes
    :: forall a
    .  Strategy.ExecutionGraph (ClaimState.ClaimState a) (AppliedRule Goal)
    -> MultiOr.MultiOr a
unprovenNodes executionGraph =
    MultiOr.MultiOr
    $ mapMaybe ClaimState.extractUnproven
    $ Strategy.pickFinal executionGraph

{- | Does the 'Strategy.ExecutionGraph' indicate a successful proof?
 -}
proven
    :: forall a
    .  Strategy.ExecutionGraph (ClaimState.ClaimState a) (AppliedRule Goal)
    -> Bool
proven = Foldable.null . unprovenNodes
