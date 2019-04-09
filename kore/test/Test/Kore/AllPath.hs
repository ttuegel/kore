module Test.Kore.AllPath where

import Test.Tasty
import Test.Tasty.HUnit

import qualified Data.Foldable as Foldable
import           Data.Function
                 ( (&) )
import qualified Data.Graph.Inductive as Gr

import qualified Kore.AllPath as AllPath
import qualified Kore.Step.Representation.MultiOr as MultiOr
import qualified Kore.Step.Strategy as Strategy

import Test.Kore.Comparators ()
import Test.Terse

type ExecutionGraph = Strategy.ExecutionGraph (AllPath.ProofState Integer) ()

emptyExecutionGraph :: AllPath.ProofState Integer -> ExecutionGraph
emptyExecutionGraph = Strategy.emptyExecutionGraph

insNode
    :: (Gr.Node, AllPath.ProofState Integer)
    -> ExecutionGraph
    -> ExecutionGraph
insNode lnode Strategy.ExecutionGraph { root, graph } =
    Strategy.ExecutionGraph { root, graph = Gr.insNode lnode graph }

insEdge
    :: (Gr.Node, Gr.Node)
    -> ExecutionGraph
    -> ExecutionGraph
insEdge (fromNode, toNode) Strategy.ExecutionGraph { root, graph } =
    Strategy.ExecutionGraph { root, graph = Gr.insEdge (fromNode, toNode, mempty) graph }

test_unprovenNodes :: [TestTree]
test_unprovenNodes =
    [ AllPath.unprovenNodes
        (emptyExecutionGraph AllPath.Proven)
        `satisfies_`
        Foldable.null
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 1))
        `satisfies_`
        (not . Foldable.null)
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 1))
        `equals`
        (MultiOr.MultiOr [1])
        $  "returns single unproven node"
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 0)
            & insNode (1, AllPath.GoalLHS 1)
            & insNode (2, AllPath.Proven)
        )
        `equals_`
        (MultiOr.MultiOr [0, 1])
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 0)
            & insNode (1, AllPath.GoalLHS 1)
            & insEdge (0, 1)
            & insNode (2, AllPath.Proven)
            & insEdge (0, 2)
        )
        `equals_`
        (MultiOr.MultiOr [1])
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 0)
            & insNode (1, AllPath.GoalLHS 1)
            & insEdge (0, 1)
            & insNode (2, AllPath.GoalLHS 2)
            & insEdge (1, 2)
            & insNode (3, AllPath.Proven)
            & insEdge (2, 3)
        )
        `equals_`
        (MultiOr.MultiOr [])
    , AllPath.unprovenNodes
        (emptyExecutionGraph (AllPath.GoalLHS 0)
            & insNode (1, AllPath.GoalRemLHS 1)
            & insEdge (0, 1)
            & insNode (2, AllPath.Proven)
            & insEdge (0, 2)
        )
        `equals_`
        (MultiOr.MultiOr [1])
    ]
