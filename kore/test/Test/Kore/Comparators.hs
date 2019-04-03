{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Test.Kore.Comparators
Description : Declares various data types involved in testing as instances of
              the 'EqualWithExplanation' class.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Test.Kore.Comparators where

import           Control.Applicative
                 ( Alternative (..) )
import           Control.Comonad.Trans.Cofree
                 ( Cofree, CofreeF (..), CofreeT (..) )
import qualified Data.Function as Function
import           Data.Functor.Classes
import           Data.Functor.Identity
                 ( Identity (..) )
import           Numeric.Natural
                 ( Natural )

import qualified Kore.Annotation.Null as Annotation
import           Kore.Annotation.Valid
import           Kore.AST.Kore
import           Kore.AST.Pure
import           Kore.AST.Sentence
import qualified Kore.Attribute.Axiom as Attribute
import           Kore.Domain.Builtin
import           Kore.Error
import           Kore.OnePath.Step
                 ( StrategyPattern )
import           Kore.OnePath.Step as StrategyPattern
                 ( StrategyPattern (..) )
import           Kore.Predicate.Predicate
import           Kore.Proof.Functional
import           Kore.Step.Axiom.Data as AttemptedAxiom
                 ( AttemptedAxiom (..) )
import           Kore.Step.Axiom.Data as AttemptedAxiomResults
                 ( AttemptedAxiomResults (..) )
import           Kore.Step.Axiom.Identifier
                 ( AxiomIdentifier )
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
import           Kore.Step.Error
import qualified Kore.Step.Error as Step.Error
import           Kore.Step.Pattern
import qualified Kore.Step.PatternAttributesError as PatternAttributesError
import           Kore.Step.Proof
import           Kore.Step.Representation.ExpandedPattern
                 ( Predicated (..) )
import           Kore.Step.Representation.MultiOr
import           Kore.Step.Rule
                 ( RulePattern (..) )
import           Kore.Step.Simplification.Data
                 ( SimplificationProof )
import           Kore.Step.Step
import qualified Kore.Step.Step as OrStepResult
                 ( OrStepResult (..) )
import           Kore.Unification.Error
import           Kore.Unification.Substitution
                 ( Substitution )
import qualified Kore.Unification.Substitution as Substitution
import           Kore.Unification.Unifier
import           Kore.Variables.Target

import Test.Tasty.HUnit.Extensions

instance EqualWithExplanation () where
    compareWithExplanation () () = Nothing
    printWithExplanation = show

instance EqualWithExplanation Natural where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

{-# ANN module ("HLint: ignore Use record patterns" :: String) #-}

instance
    ( EqualWithExplanation child
    , Eq child
    , Eq level
    , Show child
    , Eq1 domain
    , Show1 domain
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (domain child)
    , Eq (variable level)
    , Show (variable level)
    )
    => SumEqualWithExplanation (Pattern level domain variable child)
  where
    sumConstructorPair (AndPattern a1) (AndPattern a2) =
        SumConstructorSameWithArguments (EqWrap "AndPattern" a1 a2)
    sumConstructorPair pattern1@(AndPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (ApplicationPattern a1) (ApplicationPattern a2) =
        SumConstructorSameWithArguments (EqWrap "ApplicationPattern" a1 a2)
    sumConstructorPair pattern1@(ApplicationPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (BottomPattern a1) (BottomPattern a2) =
        SumConstructorSameWithArguments (EqWrap "BottomPattern" a1 a2)
    sumConstructorPair pattern1@(BottomPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (CeilPattern a1) (CeilPattern a2) =
        SumConstructorSameWithArguments (EqWrap "CeilPattern" a1 a2)
    sumConstructorPair pattern1@(CeilPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (DomainValuePattern a1) (DomainValuePattern a2) =
        SumConstructorSameWithArguments (EqWrap "DomainValuePattern" a1 a2)
    sumConstructorPair pattern1@(DomainValuePattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (EqualsPattern a1) (EqualsPattern a2) =
        SumConstructorSameWithArguments (EqWrap "EqualsPattern" a1 a2)
    sumConstructorPair pattern1@(EqualsPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (ExistsPattern a1) (ExistsPattern a2) =
        SumConstructorSameWithArguments (EqWrap "ExistsPattern" a1 a2)
    sumConstructorPair pattern1@(ExistsPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (FloorPattern a1) (FloorPattern a2) =
        SumConstructorSameWithArguments (EqWrap "FloorPattern" a1 a2)
    sumConstructorPair pattern1@(FloorPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (ForallPattern a1) (ForallPattern a2) =
        SumConstructorSameWithArguments (EqWrap "ForallPattern" a1 a2)
    sumConstructorPair pattern1@(ForallPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (IffPattern a1) (IffPattern a2) =
        SumConstructorSameWithArguments (EqWrap "IffPattern" a1 a2)
    sumConstructorPair pattern1@(IffPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (ImpliesPattern a1) (ImpliesPattern a2) =
        SumConstructorSameWithArguments (EqWrap "ImpliesPattern" a1 a2)
    sumConstructorPair pattern1@(ImpliesPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (InPattern a1) (InPattern a2) =
        SumConstructorSameWithArguments (EqWrap "InPattern" a1 a2)
    sumConstructorPair pattern1@(InPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (NextPattern a1) (NextPattern a2) =
        SumConstructorSameWithArguments (EqWrap "NextPattern" a1 a2)
    sumConstructorPair pattern1@(NextPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (NotPattern a1) (NotPattern a2) =
        SumConstructorSameWithArguments (EqWrap "NotPattern" a1 a2)
    sumConstructorPair pattern1@(NotPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (OrPattern a1) (OrPattern a2) =
        SumConstructorSameWithArguments (EqWrap "OrPattern" a1 a2)
    sumConstructorPair pattern1@(OrPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (RewritesPattern a1) (RewritesPattern a2) =
        SumConstructorSameWithArguments (EqWrap "RewritesPattern" a1 a2)
    sumConstructorPair pattern1@(RewritesPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (StringLiteralPattern a1) (StringLiteralPattern a2) =
        SumConstructorSameWithArguments (EqWrap "StringLiteralPattern" a1 a2)
    sumConstructorPair pattern1@(StringLiteralPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (CharLiteralPattern a1) (CharLiteralPattern a2) =
        SumConstructorSameWithArguments (EqWrap "CharLiteralPattern" a1 a2)
    sumConstructorPair pattern1@(CharLiteralPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (TopPattern a1) (TopPattern a2) =
        SumConstructorSameWithArguments (EqWrap "TopPattern" a1 a2)
    sumConstructorPair pattern1@(TopPattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

    sumConstructorPair (VariablePattern a1) (VariablePattern a2) =
        SumConstructorSameWithArguments (EqWrap "VariablePattern" a1 a2)
    sumConstructorPair pattern1@(VariablePattern _) pattern2 =
        SumConstructorDifferent
            (printWithExplanation pattern1) (printWithExplanation pattern2)

instance
    ( EqualWithExplanation child
    , Eq child, Eq level, Eq (variable level)
    , Show child
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (domain child)
    , Show (variable level)
    , Show1 domain
    , Eq1 domain
    ) => EqualWithExplanation (Pattern level domain variable child)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (PurePattern level domain variable annotation)
    , Show1 domain
    , Eq1 domain
    , Show (variable level)
    , Eq (variable level)
    , EqualWithExplanation (variable level)
    , Show annotation
    , Eq annotation
    , Eq level
    , EqualWithExplanation annotation
    , EqualWithExplanation (domain (Cofree (Pattern level domain variable) annotation))
    ) =>
    EqualWithExplanation (PurePattern level domain variable annotation)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (PurePattern level domain variable annotation)
    , Show1 domain
    , Eq1 domain
    , Show (variable level)
    , Eq (variable level)
    , EqualWithExplanation (variable level)
    , Show annotation
    , Eq annotation
    , Eq level
    , EqualWithExplanation annotation
    , EqualWithExplanation (domain (Cofree (Pattern level domain variable) annotation))
    ) =>
    WrapperEqualWithExplanation (PurePattern level domain variable annotation)
  where
    wrapperField expected actual =
        EqWrap
            "getPurePattern = "
            (getPurePattern expected)
            (getPurePattern actual)
    wrapperConstructorName _ = "PurePattern"

instance
    ( Show (KorePattern domain variable annotation)
    , Show1 domain
    , Eq1 domain
    , Show annotation
    , Eq annotation
    , EqualWithExplanation annotation
    , EqualWithExplanation (variable Meta)
    , EqualWithExplanation (variable Object)
    , EqualWithExplanation (domain (Cofree (UnifiedPattern domain variable) annotation))
    , OrdMetaOrObject variable
    , ShowMetaOrObject variable
    ) =>
    EqualWithExplanation (KorePattern domain variable annotation)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (KorePattern domain variable annotation)
    , Eq1 domain, Show1 domain
    , Eq annotation, Show annotation
    , EqualWithExplanation annotation
    , EqualWithExplanation (variable Meta)
    , EqualWithExplanation (variable Object)
    , EqualWithExplanation (domain (Cofree (UnifiedPattern domain variable) annotation))
    , OrdMetaOrObject variable, ShowMetaOrObject variable
    ) =>
    WrapperEqualWithExplanation (KorePattern domain variable annotation)
  where
    wrapperField expected actual =
        EqWrap
            "getKorePattern = "
            (getKorePattern expected)
            (getKorePattern actual)
    wrapperConstructorName _ = "KorePattern"

instance
    ( Show (CofreeT f w a)
    , EqualWithExplanation (w (CofreeF f a (CofreeT f w a)))
    ) =>
    EqualWithExplanation (CofreeT f w a)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (CofreeT f w a)
    , EqualWithExplanation (w (CofreeF f a (CofreeT f w a)))
    ) =>
    WrapperEqualWithExplanation (CofreeT f w a)
  where
    wrapperField expected actual =
        EqWrap "runCofreeT = " (runCofreeT expected) (runCofreeT actual)
    wrapperConstructorName _ = "CofreeT"

instance
    ( EqualWithExplanation a, Show a ) =>
    EqualWithExplanation (Identity a)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    ( EqualWithExplanation a, Show a ) =>
    WrapperEqualWithExplanation (Identity a)
  where
    wrapperField expected actual =
        EqWrap "runIdentity = " (runIdentity expected) (runIdentity actual)
    wrapperConstructorName _ = "Identity"

instance
    ( Show a, EqualWithExplanation a
    , Show (f b), EqualWithExplanation (f b)
    ) =>
    EqualWithExplanation (CofreeF f a b)
  where
    compareWithExplanation (a1 :< fb1) (a2 :< fb2) =
        compareWithExplanation fb1 fb2 <|> compareWithExplanation a1 a2
    printWithExplanation = show

instance
    ( Eq level, Show level
    , Eq (variable level), Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => SumEqualWithExplanation (StepProof level variable)
  where
    sumConstructorPair (StepProof a1) (StepProof a2) =
        SumConstructorSameWithArguments (EqWrap "StepProofCombined" a1 a2)

instance
    ( Eq level, Show level
    , Eq (variable level), Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => SumEqualWithExplanation (StepProofAtom level variable)
  where
    sumConstructorPair (StepProofUnification a1) (StepProofUnification a2) =
        SumConstructorSameWithArguments (EqWrap "StepProofUnification" a1 a2)
    sumConstructorPair a1@(StepProofUnification _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (StepProofVariableRenamings a1) (StepProofVariableRenamings a2)
      =
        SumConstructorSameWithArguments
            (EqWrap "StepProofVariableRenamings" a1 a2)
    sumConstructorPair a1@(StepProofVariableRenamings _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (StepProofSimplification a1)
        (StepProofSimplification a2)
      =
        SumConstructorSameWithArguments (EqWrap "StepProofSimplification" a1 a2)
    sumConstructorPair a1@(StepProofSimplification _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    ( Eq level, Show level
    , Eq (variable level), Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (StepProofAtom level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq level, Show level
    , Eq (variable level), Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (StepProof level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance (Show child, EqualWithExplanation child)
    => StructEqualWithExplanation (And level child)
  where
    structFieldsWithNames
        expected@(And _ _ _)
        actual@(And _ _ _)
      = [ EqWrap
            "andSort = "
            (andSort expected)
            (andSort actual)
        , EqWrap
            "andFirst = "
            (andFirst expected)
            (andFirst actual)
        , EqWrap
            "andSecond = "
            (andSecond expected)
            (andSecond actual)
        ]
    structConstructorName _ = "And"

instance (Show child, EqualWithExplanation child)
    => EqualWithExplanation (And level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance (Show child, EqualWithExplanation child)
    => StructEqualWithExplanation (Application level child)
  where
    structFieldsWithNames
        expected@(Application _ _)
        actual@(Application _ _)
      = [ EqWrap
            "applicationSymbolOrAlias = "
            (applicationSymbolOrAlias expected)
            (applicationSymbolOrAlias actual)
        , EqWrap
            "applicationChildren = "
            (applicationChildren expected)
            (applicationChildren actual)
        ]
    structConstructorName _ = "Application"

instance (Show child, EqualWithExplanation child)
    => EqualWithExplanation (Application level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Bottom level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance (Show child, EqualWithExplanation child)
    => StructEqualWithExplanation (Ceil level child)
  where
    structFieldsWithNames
        expected@(Ceil _ _ _)
        actual@(Ceil _ _ _)
      = [ EqWrap
            "ceilOperandSort = "
            (ceilOperandSort expected)
            (ceilOperandSort actual)
        , EqWrap
            "ceilResultSort = "
            (ceilResultSort expected)
            (ceilResultSort actual)
        , EqWrap
            "ceilChild = "
            (ceilChild expected)
            (ceilChild actual)
        ]
    structConstructorName _ = "Ceil"
instance (EqualWithExplanation child, Show child)
    => EqualWithExplanation (Ceil level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    (Eq child, Show child, Eq1 domain, Show1 domain) =>
    EqualWithExplanation (DomainValue level domain child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance (Show child, EqualWithExplanation child)
    => StructEqualWithExplanation (Equals level child)
  where
    structFieldsWithNames
        expected@(Equals _ _ _ _)
        actual@(Equals _ _ _ _)
      = [ EqWrap
            "equalsResultSort = "
            (equalsResultSort expected)
            (equalsResultSort actual)
        , EqWrap
            "equalsOperandSort = "
            (equalsOperandSort expected)
            (equalsOperandSort actual)
        , EqWrap
            "equalsFirst = "
            (equalsFirst expected)
            (equalsFirst actual)
        , EqWrap
            "equalsSecond = "
            (equalsSecond expected)
            (equalsSecond actual)
        ]
    structConstructorName _ = "Equals"

instance (Show child, EqualWithExplanation child)
    => EqualWithExplanation (Equals level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq child
    , Eq (variable level)
    , Show child
    , Show (variable level)
    , EqualWithExplanation child
    , EqualWithExplanation (variable level)
    )
    => StructEqualWithExplanation (Exists level variable child)
  where
    structFieldsWithNames
        expected@(Exists _ _ _)
        actual@(Exists _ _ _)
      = [ EqWrap
            "existsSort = "
            (existsSort expected)
            (existsSort actual)
        , EqWrap
            "existsVariable = "
            (existsVariable expected)
            (existsVariable actual)
        , EqWrap
            "existsChild = "
            (existsChild expected)
            (existsChild actual)
        ]
    structConstructorName _ = "Exists"
instance
    ( EqualWithExplanation child
    , Eq child
    , Show child
    , EqualWithExplanation (variable level)
    , Eq (variable level)
    , Show (variable level)
    ) => EqualWithExplanation (Exists level variable child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance (Show child, EqualWithExplanation child)
    => StructEqualWithExplanation (Floor level child)
  where
    structFieldsWithNames
        expected@(Floor _ _ _)
        actual@(Floor _ _ _)
      = [ EqWrap
            "floorOperandSort = "
            (floorOperandSort expected)
            (floorOperandSort actual)
        , EqWrap
            "floorResultSort = "
            (floorResultSort expected)
            (floorResultSort actual)
        , EqWrap
            "floorChild = "
            (floorChild expected)
            (floorChild actual)
        ]
    structConstructorName _ = "Floor"
instance (Show child, EqualWithExplanation child)
    => EqualWithExplanation (Floor level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq child
    , Eq (variable level)
    , Show child
    , Show (variable level)
    , EqualWithExplanation child
    , EqualWithExplanation (variable level)
    )
    => StructEqualWithExplanation (Forall level variable child)
  where
    structFieldsWithNames
        expected@(Forall _ _ _)
        actual@(Forall _ _ _)
      = [ EqWrap
            "forallSort = "
            (forallSort expected)
            (forallSort actual)
        , EqWrap
            "forallVariable = "
            (forallVariable expected)
            (forallVariable actual)
        , EqWrap
            "forallChild = "
            (forallChild expected)
            (forallChild actual)
        ]
    structConstructorName _ = "Forall"
instance
    ( EqualWithExplanation child
    , Eq child
    , Show child
    , EqualWithExplanation (variable level)
    , Eq (variable level)
    , Show (variable level)
    ) => EqualWithExplanation (Forall level variable child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq child
    , Show child
    , EqualWithExplanation child
    )
    => StructEqualWithExplanation (Iff level child)
  where
    structFieldsWithNames
        expected@(Iff _ _ _)
        actual@(Iff _ _ _)
      = [ EqWrap
            "iffSort = "
            (iffSort expected)
            (iffSort actual)
        , EqWrap
            "iffFirst = "
            (iffFirst expected)
            (iffFirst actual)
        , EqWrap
            "iffSecond = "
            (iffSecond expected)
            (iffSecond actual)
        ]
    structConstructorName _ = "Iff"
instance
    ( EqualWithExplanation child
    , Eq child
    , Show child
    ) => EqualWithExplanation (Iff level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Implies level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show
instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (In level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show
instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Next level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance (Show child, Eq child, EqualWithExplanation child)
  =>
    StructEqualWithExplanation (Not level child)
  where
    structFieldsWithNames
        expected@(Not _ _)
        actual@(Not _ _)
      = [ EqWrap
            "notSort = "
            (notSort expected)
            (notSort actual)
        , EqWrap
            "notChild = "
            (notChild expected)
            (notChild actual)
        ]
    structConstructorName _ = "Not"
instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Not level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance (Show child, Eq child, EqualWithExplanation child)
  =>
    StructEqualWithExplanation (Or level child)
  where
    structFieldsWithNames
        expected@(Or _ _ _)
        actual@(Or _ _ _)
      = [ EqWrap
            "orSort = "
            (orSort expected)
            (orSort actual)
        , EqWrap
            "orFirst = "
            (orFirst expected)
            (orFirst actual)
        , EqWrap
            "orSecond = "
            (orSecond expected)
            (orSecond actual)
        ]
    structConstructorName _ = "Or"
instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Or level child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Rewrites level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show
instance EqualWithExplanation StringLiteral
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show
instance EqualWithExplanation CharLiteral
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show
instance
    (EqualWithExplanation child, Eq child, Show child)
    => EqualWithExplanation (Top level child)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation (Variable level)
  where
    structFieldsWithNames
        expected@(Variable _ _ _)
        actual@(Variable _ _ _)
      = [ EqWrap
            "variableName = "
            (variableName expected)
            (variableName actual)
        , EqWrap
            "variableCounter = "
            (variableCounter expected)
            (variableCounter actual)
        , EqWrap
            "variableSort = "
            (variableSort expected)
            (variableSort actual)
        ]
    structConstructorName _ = "Variable"

instance EqualWithExplanation (Variable level) where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (Concrete level) where
    compareWithExplanation = \case {}
    printWithExplanation = \case {}

instance StructEqualWithExplanation (Id level) where
    structFieldsWithNames expected@(Id _ _) actual@(Id _ _) =
        map (\f -> f expected actual)
            [ Function.on (EqWrap "getId = ") getIdForError
            , Function.on (EqWrap "idLocation = ") (const ())
            ]
    structConstructorName _ = "Id"

instance EqualWithExplanation (Id level)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (Sort level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation (SymbolOrAlias level)
    where
      structFieldsWithNames
          expected@(SymbolOrAlias _ _)
          actual@(SymbolOrAlias _ _)
        = [ EqWrap
              "symbolOrAliasConstructor = "
              (symbolOrAliasConstructor expected)
              (symbolOrAliasConstructor actual)
          , EqWrap
              "symbolOrAliasParams = "
              (symbolOrAliasParams expected)
              (symbolOrAliasParams actual)
          ]
      structConstructorName _ = "SymbolOrAlias"
instance EqualWithExplanation (SymbolOrAlias level)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance SumEqualWithExplanation UnificationError
  where
    sumConstructorPair UnsupportedPatterns UnsupportedPatterns =
        SumConstructorSameNoArguments

instance EqualWithExplanation UnificationError
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (ClashReason level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance (Show (variable level), EqualWithExplanation (variable level))
    => SumEqualWithExplanation (SubstitutionError level variable)
  where
    sumConstructorPair
        (NonCtorCircularVariableDependency a1)
        (NonCtorCircularVariableDependency a2)
      =
        SumConstructorSameWithArguments
            (EqWrap "NonCtorCircularVariableDependency" a1 a2)


instance (Show (variable level), EqualWithExplanation (variable level))
    => EqualWithExplanation (SubstitutionError level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Ord (variable Object), Show (variable Object)
    , EqualWithExplanation (variable Object)
    ) =>
    SumEqualWithExplanation (StepError Object variable)
  where
    sumConstructorPair (StepErrorUnification a1) (StepErrorUnification a2) =
        SumConstructorSameWithArguments (EqWrap "StepErrorUnification" a1 a2)
    sumConstructorPair a1@(StepErrorUnification _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair (StepErrorSubstitution a1) (StepErrorSubstitution a2) =
        SumConstructorSameWithArguments (EqWrap "StepErrorSubstitution" a1 a2)
    sumConstructorPair a1@(StepErrorSubstitution _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (StepErrorUnsupportedSymbolic a)
        (StepErrorUnsupportedSymbolic b)
      =
        SumConstructorSameWithArguments
        $ EqWrap "StepErrorUnsupportedSymbolic" a b

    sumConstructorPair a1@(StepErrorUnsupportedSymbolic _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    ( Ord (variable Object), Show (variable Object)
    , EqualWithExplanation (variable Object)
    ) =>
    EqualWithExplanation (StepError Object variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( EqualWithExplanation (variable Object)
    , Ord (variable Object), Show (variable Object)
    ) =>
    EqualWithExplanation (Step.Error.UnsupportedSymbolic Object variable)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( EqualWithExplanation (variable Object)
    , Ord (variable Object), Show (variable Object)
    ) =>
    StructEqualWithExplanation (Step.Error.UnsupportedSymbolic Object variable)
  where
    structConstructorName _ = "UnsupportedSymbolic"
    structFieldsWithNames expect actual =
        map (\f -> f expect actual)
            [ Function.on (EqWrap "unification = ") Step.Error.unification
            , Function.on (EqWrap "rule = "       ) Step.Error.rule
            ]

instance
    ( Eq (variable level)
    , Show (variable level)
    )
    => EqualWithExplanation (FunctionalProof level variable)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq (variable level)
    , Show (variable level)
    )
    => EqualWithExplanation (FunctionProof level variable)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance
    ( Eq level, Eq (variable level)
    , Show level, Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => SumEqualWithExplanation (UnificationProof level variable)
  where
    sumConstructorPair EmptyUnificationProof EmptyUnificationProof =
        SumConstructorSameNoArguments
    sumConstructorPair a1@EmptyUnificationProof a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (CombinedUnificationProof a1) (CombinedUnificationProof a2)
      =
        SumConstructorSameWithArguments
            (EqWrap "CombinedUnificationProof" a1 a2)
    sumConstructorPair a1@(CombinedUnificationProof _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair (ConjunctionIdempotency a1) (ConjunctionIdempotency a2) =
        SumConstructorSameWithArguments (EqWrap "ConjunctionIdempotency" a1 a2)
    sumConstructorPair a1@(ConjunctionIdempotency _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (Proposition_5_24_3 a1 a2 a3) (Proposition_5_24_3 b1 b2 b3)
      =
        SumConstructorSameWithArguments
            (EqWrap "Proposition_5_24_3" (a1, a2, a3) (b1, b2, b3))
    sumConstructorPair a1@(Proposition_5_24_3 _ _ _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (AndDistributionAndConstraintLifting a1 a2)
        (AndDistributionAndConstraintLifting b1 b2) =
            SumConstructorSameWithArguments
                (EqWrap "AndDistributionAndConstraintLifting" (a1, a2) (b1, b2))
    sumConstructorPair a1@(AndDistributionAndConstraintLifting _ _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (SubstitutionMerge a1 a2 a3) (SubstitutionMerge b1 b2 b3)
      =
        SumConstructorSameWithArguments
            (EqWrap "SubstitutionMerge" (a1, a2, a3) (b1, b2, b3))
    sumConstructorPair a1@(SubstitutionMerge _ _ _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    ( Eq level, Eq (variable level)
    , Show level, Show (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (UnificationProof level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => StructEqualWithExplanation (VariableRenaming level variable)
  where
    structFieldsWithNames
        expected@(VariableRenaming _ _)
        actual@(VariableRenaming _ _)
      = [ EqWrap
            "variableRenamingOriginal = "
            (variableRenamingOriginal expected)
            (variableRenamingOriginal actual)
        , EqWrap
            "variableRenamingRenamed = "
            (variableRenamingRenamed expected)
            (variableRenamingRenamed actual)
        ]
    structConstructorName _ = "VariableRenaming"

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => EqualWithExplanation (VariableRenaming level variable)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => SumEqualWithExplanation (Target variable level)
  where
    sumConstructorPair (Target a1) (Target a2) =
        SumConstructorSameWithArguments (EqWrap "Target" a1 a2)
    sumConstructorPair a1@(Target _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair (NonTarget a1) (NonTarget a2) =
        SumConstructorSameWithArguments (EqWrap "NonTarget" a1 a2)
    sumConstructorPair a1@(NonTarget _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => EqualWithExplanation (Target variable level)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    )
    => EqualWithExplanation (Substitution level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation child, Show child) =>
    EqualWithExplanation (Builtin child)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (External child) where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation (External child) where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "domainValueSort = ")
            Kore.Domain.Builtin.domainValueSort
            expect
            actual
        , Function.on (EqWrap "domainValueChild = ")
            Kore.Domain.Builtin.domainValueChild
            expect
            actual
        ]
    structConstructorName _ = "External"

instance EqualWithExplanation InternalInt where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation InternalInt where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "builtinIntSort = ") builtinIntSort expect actual
        , Function.on (EqWrap "builtinIntValue = ") builtinIntValue expect actual
        ]
    structConstructorName _ = "InternalInt"

instance EqualWithExplanation InternalBool where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation InternalBool where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "builtinBoolSort = ") builtinBoolSort expect actual
        , Function.on (EqWrap "builtinBoolValue = ") builtinBoolValue expect actual
        ]
    structConstructorName _ = "InternalBool"

instance
    (EqualWithExplanation child, Show child) =>
    EqualWithExplanation (InternalMap child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation child, Show child) =>
    StructEqualWithExplanation (InternalMap child)
  where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "builtinMapSort = ") builtinMapSort expect actual
        , Function.on (EqWrap "builtinMapUnit = ") builtinMapUnit expect actual
        , Function.on (EqWrap "builtinMapElement = ") builtinMapElement expect actual
        , Function.on (EqWrap "builtinMapConcat = ") builtinMapConcat expect actual
        , Function.on (EqWrap "builtinMapChild = ") builtinMapChild expect actual
        ]
    structConstructorName _ = "InternalMap"

instance
    (EqualWithExplanation child, Show child) =>
    EqualWithExplanation (InternalList child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation child, Show child) =>
    StructEqualWithExplanation (InternalList child)
  where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "builtinListSort = ") builtinListSort expect actual
        , Function.on (EqWrap "builtinListUnit = ") builtinListUnit expect actual
        , Function.on (EqWrap "builtinListElement = ") builtinListElement expect actual
        , Function.on (EqWrap "builtinListConcat = ") builtinListConcat expect actual
        , Function.on (EqWrap "builtinListChild = ") builtinListChild expect actual
        ]
    structConstructorName _ = "InternalList"

instance EqualWithExplanation InternalSet where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation InternalSet where
    structFieldsWithNames expect actual =
        [ Function.on (EqWrap "builtinSetSort = ") builtinSetSort expect actual
        , Function.on (EqWrap "builtinSetUnit = ") builtinSetUnit expect actual
        , Function.on (EqWrap "builtinSetElement = ") builtinSetElement expect actual
        , Function.on (EqWrap "builtinSetConcat = ") builtinSetConcat expect actual
        , Function.on (EqWrap "builtinSetChild = ") builtinSetChild expect actual
        ]
    structConstructorName _ = "InternalSet"

instance
    (EqualWithExplanation child, Show child) =>
    SumEqualWithExplanation (Builtin child)
  where
    sumConstructorPair (BuiltinExternal ext1) (BuiltinExternal ext2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinExternal" ext1 ext2)
    sumConstructorPair (BuiltinInt int1) (BuiltinInt int2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinInt" int1 int2)
    sumConstructorPair (BuiltinBool bool1) (BuiltinBool bool2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinBool" bool1 bool2)
    sumConstructorPair (BuiltinMap map1) (BuiltinMap map2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinMap" map1 map2)
    sumConstructorPair (BuiltinList list1) (BuiltinList list2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinList" list1 list2)
    sumConstructorPair (BuiltinSet set1) (BuiltinSet set2) =
        SumConstructorSameWithArguments
            (EqWrap "BuiltinSet" set1 set2)
    sumConstructorPair a b =
        SumConstructorDifferent
            (printWithExplanation a)
            (printWithExplanation b)

instance
    ( EqualWithExplanation (variable level)
    , Show level, Show (variable level)
    , Eq level, Eq (variable level)
    )
    => SumEqualWithExplanation (Substitution level variable)
  where
    sumConstructorPair s1 s2
        | s1Norm && s2Norm
            = SumConstructorSameWithArguments
                (EqWrap "NormalizedSubstitution" s1Inner s2Inner)
        | not s1Norm && not s2Norm
            = SumConstructorSameWithArguments
                (EqWrap "Substitution" s1Inner s2Inner)
        | otherwise =
            SumConstructorDifferent
                (printWithExplanation s1)
                (printWithExplanation s2)
      where
        s1Norm = Substitution.isNormalized s1
        s2Norm = Substitution.isNormalized s2
        s1Inner = Substitution.unwrap s1
        s2Inner = Substitution.unwrap s2

instance
    ( Show level, Show (variable level), Show child
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation child
    , EqualWithExplanation (StepPattern level variable)
    )
    => StructEqualWithExplanation (Predicated level variable child)
  where
    structFieldsWithNames
        expected@(Predicated _ _ _) actual@(Predicated _ _ _)
      =
        [ EqWrap
            "term = "
            (term expected)
            (term actual)
        , EqWrap
            "predicate = "
            (predicate expected)
            (predicate actual)
        , EqWrap
            "substitution = "
            (substitution expected)
            (substitution actual)
        ]
    structConstructorName _ = "Predicated"

instance
    ( Show level, Show (variable level), Show child
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation child
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (Predicated level variable child)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( EqualWithExplanation (variable level)
    , Eq level, Show level
    , Eq (variable level), Show (variable level)
    )
    => EqualWithExplanation (Predicate level variable)
  where
    compareWithExplanation p1 p2 = do
        compared <- traverse (\x -> traverse (compareWithExplanation x) p2) p1
        return $
            "Predicate ("
            ++ stringFromPredicate (compactPredicatePredicate compared)
            ++ ")"
    printWithExplanation = show

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    )
    => StructEqualWithExplanation (AttemptedAxiomResults level variable)
  where
    structFieldsWithNames
        expected@(AttemptedAxiomResults _ _)
        actual@(AttemptedAxiomResults _ _)
      =
        [ EqWrap
            "results = "
            (results expected)
            (results actual)
        , EqWrap
            "remainders = "
            (remainders expected)
            (remainders actual)
        ]
    structConstructorName _ = "AttemptedAxiomResults"

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    )
    => EqualWithExplanation (AttemptedAxiomResults level variable)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => SumEqualWithExplanation (AttemptedAxiom level variable)
  where
    sumConstructorPair
        AttemptedAxiom.NotApplicable
        AttemptedAxiom.NotApplicable
      =
        SumConstructorSameNoArguments
    sumConstructorPair a1@AttemptedAxiom.NotApplicable a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair
        (AttemptedAxiom.Applied a1) (AttemptedAxiom.Applied a2)
      =
        SumConstructorSameWithArguments
            (EqWrap "Applied" a1 a2)
    sumConstructorPair a1@(AttemptedAxiom.Applied _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (AttemptedAxiom level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (SimplificationProof level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance
    (Show a, EqualWithExplanation a)
    => SumEqualWithExplanation (MultiOr a)
  where
    sumConstructorPair (MultiOr a1) (MultiOr a2)
      =
        SumConstructorSameWithArguments
            (EqWrap "MultiOr" a1 a2)

instance
    (Show a, EqualWithExplanation a)
    => EqualWithExplanation (MultiOr a)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (PatternAttributesError.FunctionError level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (PatternAttributesError.FunctionalError level)
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => SumEqualWithExplanation (UnificationOrSubstitutionError level variable)
  where
    sumConstructorPair (UnificationError a1) (UnificationError a2) =
        SumConstructorSameWithArguments (EqWrap "UnificationError" a1 a2)
    sumConstructorPair a1@(UnificationError _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

    sumConstructorPair (SubstitutionError a1) (SubstitutionError a2) =
        SumConstructorSameWithArguments (EqWrap "SubstitutionError" a1 a2)
    sumConstructorPair a1@(SubstitutionError _) a2 =
        SumConstructorDifferent
            (printWithExplanation a1) (printWithExplanation a2)

instance
    (EqualWithExplanation (variable level), Show (variable level))
    => EqualWithExplanation (UnificationOrSubstitutionError level variable)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (variable Object), Show (variable Meta), Show child
    , Eq (variable Meta), Eq (variable Object), Eq child
    , EqualWithExplanation (variable Meta)
    , EqualWithExplanation (variable Object)
    , EqualWithExplanation (domain child)
    , EqualWithExplanation child
    , Show1 domain
    , Eq1 domain
    )
    => EqualWithExplanation (UnifiedPattern domain variable child)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Show (variable Object), Show (variable Meta), Show child
    , Eq (variable Meta), Eq (variable Object), Eq child
    , EqualWithExplanation (variable Object)
    , EqualWithExplanation (variable Meta)
    , EqualWithExplanation (domain child)
    , EqualWithExplanation child
    , Show1 domain
    , Eq1 domain
    )
    => SumEqualWithExplanation (UnifiedPattern domain variable child)
  where
    sumConstructorPair (UnifiedObjectPattern p1) (UnifiedObjectPattern p2) =
        SumConstructorSameWithArguments (EqWrap "UnifiedObjectPattern" p1 p2)

instance
    ( EqualWithExplanation (a Meta)
    , EqualWithExplanation (a Object)
    , ShowMetaOrObject a
    ) =>
    SumEqualWithExplanation (Unified a)
  where
    sumConstructorPair (UnifiedObject a1) (UnifiedObject a2) =
        SumConstructorSameWithArguments (EqWrap "UnifiedObject" a1 a2)

instance
    ( EqualWithExplanation (a Meta)
    , EqualWithExplanation (a Object)
    , Show (a Meta)
    , Show (a Object)
    , SumEqualWithExplanation (Unified a)
    )
    => EqualWithExplanation (Unified a)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => StructEqualWithExplanation (OrStepResult level variable)
  where
    structFieldsWithNames
        expected@(OrStepResult _ _)
        actual@(OrStepResult _ _)
      = [ EqWrap
            "rewrittenPattern = "
            (OrStepResult.rewrittenPattern expected)
            (OrStepResult.rewrittenPattern actual)
        , EqWrap
            "remainder = "
            (OrStepResult.remainder expected)
            (OrStepResult.remainder actual)
        ]
    structConstructorName _ = "OrStepResult"

instance
    ( Show level, Show (variable level)
    , Eq level, Eq (variable level)
    , EqualWithExplanation (variable level)
    , EqualWithExplanation (StepPattern level variable)
    )
    => EqualWithExplanation (OrStepResult level variable)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( EqualWithExplanation patt
    , Show patt
    , Eq patt
    )
    => SumEqualWithExplanation (StrategyPattern patt)
  where
    sumConstructorPair
        (StrategyPattern.RewritePattern p1) (StrategyPattern.RewritePattern p2)
      =
        SumConstructorSameWithArguments (EqWrap "RewritePattern" p1 p2)
    sumConstructorPair (StrategyPattern.Stuck p1) (StrategyPattern.Stuck p2) =
        SumConstructorSameWithArguments (EqWrap "Stuck" p1 p2)
    sumConstructorPair StrategyPattern.Bottom StrategyPattern.Bottom =
        SumConstructorSameNoArguments
    sumConstructorPair p1 p2 =
        SumConstructorDifferent
            (printWithExplanation p1)
            (printWithExplanation p2)

instance
    ( Show patt
    , SumEqualWithExplanation (StrategyPattern patt)
    )
    => EqualWithExplanation (StrategyPattern patt)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation (Annotation.Null level) where
    compareWithExplanation _ _ = Nothing
    printWithExplanation = show

instance
    ( EqualWithExplanation variable, Show variable
    ) => StructEqualWithExplanation (Valid variable level)
  where
    structFieldsWithNames expected actual =
        [ EqWrap
            "patternSort = "
            (patternSort expected)
            (patternSort actual)
        , EqWrap
            "freeVariables = "
            (Kore.AST.Kore.freeVariables expected)
            (Kore.AST.Kore.freeVariables actual)
        ]
    structConstructorName _ = "Valid"

instance
    ( EqualWithExplanation variable, Show variable
    ) => EqualWithExplanation (Valid variable level)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation PatternAttributesError.ConstructorLikeError
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show

instance EqualWithExplanation ConstructorLikeProof
  where
    compareWithExplanation = rawCompareWithExplanation
    printWithExplanation = show


instance SumEqualWithExplanation (AxiomIdentifier level)
  where
    sumConstructorPair
        (AxiomIdentifier.Application p1) (AxiomIdentifier.Application p2)
      =
        SumConstructorSameWithArguments (EqWrap "Application" p1 p2)
    sumConstructorPair
        (AxiomIdentifier.Ceil p1) (AxiomIdentifier.Ceil p2)
      =
        SumConstructorSameWithArguments (EqWrap "Ceil" p1 p2)
    sumConstructorPair p1 p2 =
        SumConstructorDifferent
            (printWithExplanation p1)
            (printWithExplanation p2)

instance EqualWithExplanation (AxiomIdentifier level)
  where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance
    ( MetaOrObject level
    , Ord  (variable level)
    , Show (variable level)
    , EqualWithExplanation (variable level)
    ) =>
    EqualWithExplanation (RulePattern level variable)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    ( MetaOrObject level
    , Ord  (variable level)
    , Show (variable level)
    , EqualWithExplanation (variable level)
    ) =>
    StructEqualWithExplanation (RulePattern level variable)
  where
    structConstructorName _ = "RulePattern"
    structFieldsWithNames expect actual =
        map (\f -> f expect actual)
            [ Function.on (EqWrap "left = "      ) left
            , Function.on (EqWrap "right = "     ) right
            , Function.on (EqWrap "requires = "  ) requires
            , Function.on (EqWrap "attributes = ") attributes
            ]

instance EqualWithExplanation Attribute.Axiom where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance StructEqualWithExplanation Attribute.Axiom where
    structConstructorName _ = "Axiom"
    structFieldsWithNames expect actual =
        map (\f -> f expect actual)
            [ Function.on (EqWrap "heatCool = "       ) Attribute.heatCool
            , Function.on (EqWrap "productionID = "   ) Attribute.productionID
            , Function.on (EqWrap "assoc = "          ) Attribute.assoc
            , Function.on (EqWrap "comm = "           ) Attribute.comm
            , Function.on (EqWrap "unit = "           ) Attribute.unit
            , Function.on (EqWrap "idem = "           ) Attribute.idem
            , Function.on (EqWrap "trusted = "        ) Attribute.trusted
            , Function.on (EqWrap "concrete = "       ) Attribute.concrete
            , Function.on (EqWrap "simplification = " ) Attribute.simplification
            , Function.on (EqWrap "overload = "       ) Attribute.overload
            , Function.on (EqWrap "smtLemma = "       ) Attribute.smtLemma
            , Function.on (EqWrap "label = "          ) Attribute.label
            ]

instance EqualWithExplanation Attribute.HeatCool where
    compareWithExplanation = sumCompareWithExplanation
    printWithExplanation = show

instance SumEqualWithExplanation Attribute.HeatCool where
    sumConstructorPair Attribute.Heat   Attribute.Heat   =
        SumConstructorSameNoArguments
    sumConstructorPair Attribute.Normal Attribute.Normal =
        SumConstructorSameNoArguments
    sumConstructorPair Attribute.Cool   Attribute.Cool   =
        SumConstructorSameNoArguments
    sumConstructorPair expect           actual           =
        SumConstructorDifferent (show expect) (show actual)

instance EqualWithExplanation Attribute.ProductionID where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.ProductionID where
    wrapperConstructorName _ = "ProductionID"
    wrapperField =
        Function.on (EqWrap "getProductionID = ") Attribute.getProductionID

instance EqualWithExplanation Attribute.Assoc where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Assoc where
    wrapperConstructorName _ = "Assoc"
    wrapperField = Function.on (EqWrap "isAssoc = ") Attribute.isAssoc

instance EqualWithExplanation Attribute.Comm where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Comm where
    wrapperConstructorName _ = "Comm"
    wrapperField = Function.on (EqWrap "isComm = ") Attribute.isComm

instance EqualWithExplanation Attribute.Unit where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Unit where
    wrapperConstructorName _ = "Unit"
    wrapperField = Function.on (EqWrap "isUnit = ") Attribute.isUnit

instance EqualWithExplanation Attribute.Idem where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Idem where
    wrapperConstructorName _ = "Idem"
    wrapperField = Function.on (EqWrap "isIdem = ") Attribute.isIdem

instance EqualWithExplanation Attribute.Trusted where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Trusted where
    wrapperConstructorName _ = "Trusted"
    wrapperField = Function.on (EqWrap "isTrusted = ") Attribute.isTrusted

instance EqualWithExplanation Attribute.Concrete where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Concrete where
    wrapperConstructorName _ = "Concrete"
    wrapperField = Function.on (EqWrap "isConcrete = ") Attribute.isConcrete

instance EqualWithExplanation Attribute.Simplification where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Simplification where
    wrapperConstructorName _ = "Simplification"
    wrapperField =
        Function.on (EqWrap "isSimplification = ") Attribute.isSimplification

instance EqualWithExplanation Attribute.Overload where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Overload where
    wrapperConstructorName _ = "Overload"
    wrapperField = Function.on (EqWrap "getOverload = ") Attribute.getOverload

instance EqualWithExplanation Attribute.SmtLemma where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.SmtLemma where
    wrapperConstructorName _ = "SmtLemma"
    wrapperField = Function.on (EqWrap "isSmtLemma = ") Attribute.isSmtLemma

instance EqualWithExplanation Attribute.Label where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance WrapperEqualWithExplanation Attribute.Label where
    wrapperConstructorName _ = "Label"
    wrapperField = Function.on (EqWrap "unLabel = ") Attribute.unLabel

-- For: Alias

instance
    MetaOrObject level
    => EqualWithExplanation (Alias level)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    MetaOrObject level
    => StructEqualWithExplanation (Alias level)
  where
    structConstructorName _ = "Alias"
    structFieldsWithNames expect actual =
        map (\f -> f expect actual)
            [ Function.on (EqWrap "aliasConstructor = ") aliasConstructor
            , Function.on (EqWrap "aliasParams = ") aliasParams
            ]

-- For: SortVariable

instance
    MetaOrObject level
    => EqualWithExplanation (SortVariable level)
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    MetaOrObject level
    => WrapperEqualWithExplanation (SortVariable level)
  where
    wrapperField = Function.on (EqWrap "getSortVariable = ") getSortVariable
    wrapperConstructorName _ = "SortVariable"

-- For: Error

instance
    EqualWithExplanation (Error a)
  where
    compareWithExplanation = structCompareWithExplanation
    printWithExplanation = show

instance
    StructEqualWithExplanation (Error a)
  where
    structFieldsWithNames (Error expectedContext expectedMessage)
                          (Error actualContext   actualMessage) =
        [ EqWrap "errorMessage = " expectedMessage actualMessage
        , EqWrap "errorContext = " expectedContext actualContext
        ]
    structConstructorName _ = "Error"

-- For∷ Attributes

instance
    EqualWithExplanation Attributes
  where
    compareWithExplanation = wrapperCompareWithExplanation
    printWithExplanation = show

instance
    WrapperEqualWithExplanation Attributes
  where
    wrapperField = Function.on (EqWrap "getAttributes = ") getAttributes
    wrapperConstructorName _ = "Attributes"
