{-|
Module      : Kore.Attribute.Axiom
Description : Axiom sentence attributes
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com

-}
module Kore.Attribute.Axiom
  ( Axiom (..),
    HeatCool (..),
    ProductionID (..),
    Assoc (..),
    Comm (..),
    Unit (..),
    Idem (..),
    Trusted (..),
    Concrete (..),
    Simplification (..),
    Overload (..),
    SmtLemma (..),
    Label (..),
    SourceLocation (..),
    Constructor (..),
    RuleIndex (..)
    )
where

import Control.DeepSeq
  ( NFData
    )
import qualified Control.Monad as Monad
import Data.Default
  ( Default (..)
    )
import Data.Generics.Product
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Kore.Attribute.Assoc
import Kore.Attribute.Axiom.Concrete
import Kore.Attribute.Axiom.Constructor
import Kore.Attribute.Axiom.Unit
import Kore.Attribute.Comm
import Kore.Attribute.HeatCool
import Kore.Attribute.Idem
import Kore.Attribute.Label
import Kore.Attribute.Overload
import Kore.Attribute.Parser
  ( ParseAttributes (..)
    )
import Kore.Attribute.ProductionID
import Kore.Attribute.RuleIndex
import Kore.Attribute.Simplification
import Kore.Attribute.SmtLemma
import Kore.Attribute.SourceLocation
import Kore.Attribute.Trusted
import Kore.Debug

{- | Attributes specific to Kore axiom sentences.
 -}
data Axiom
  = Axiom
      { heatCool :: !HeatCool,
        -- ^ An axiom may be denoted as a heating or cooling rule.
        productionID :: !ProductionID,
        -- ^ The identifier from the front-end identifying a rule or group of rules.
        assoc :: !Assoc,
        -- ^ The axiom is an associativity axiom.
        comm :: !Comm,
        -- ^ The axiom is a commutativity axiom.
        unit :: !Unit,
        -- ^ The axiom is a left- or right-unit axiom.
        idem :: !Idem,
        -- ^ The axiom is an idempotency axiom.
        trusted :: !Trusted,
        -- ^ The claim is trusted
        concrete :: !Concrete,
        simplification :: !Simplification,
        -- ^ This is an axiom used for simplification
        -- (as opposed to, e.g., function evaluation).
        overload :: !Overload,
        -- ^ The axiom is an overloaded-production axiom.
        smtLemma :: !SmtLemma,
        -- ^ The axiom should be sent to SMT as a lemma.
        label :: !Label,
        -- ^ The user-defined label associated with the axiom.
        sourceLocation :: !SourceLocation,
        -- ^ Source and location in the original file.
        constructor :: !Constructor,
        -- ^ Shows that this is one of the constructor axioms
        -- (e.g. no confusion, no junk)
        identifier :: !RuleIndex
        -- ^ Used to identify an axiom in the repl.
        }
  deriving (Eq, GHC.Generic, Ord, Show)

instance SOP.Generic Axiom

instance SOP.HasDatatypeInfo Axiom

instance Debug Axiom

instance NFData Axiom

instance Default Axiom where
  def =
    Axiom
      { heatCool = def,
        productionID = def,
        assoc = def,
        comm = def,
        unit = def,
        idem = def,
        trusted = def,
        concrete = def,
        simplification = def,
        overload = def,
        smtLemma = def,
        label = def,
        sourceLocation = def,
        constructor = def,
        identifier = def
        }

instance ParseAttributes Axiom where

  parseAttribute attr =
    typed @HeatCool (parseAttribute attr)
      Monad.>=> typed @ProductionID (parseAttribute attr)
      Monad.>=> typed @Assoc (parseAttribute attr)
      Monad.>=> typed @Comm (parseAttribute attr)
      Monad.>=> typed @Unit (parseAttribute attr)
      Monad.>=> typed @Idem (parseAttribute attr)
      Monad.>=> typed @Trusted (parseAttribute attr)
      Monad.>=> typed @Concrete (parseAttribute attr)
      Monad.>=> typed @Simplification (parseAttribute attr)
      Monad.>=> typed @Overload (parseAttribute attr)
      Monad.>=> typed @SmtLemma (parseAttribute attr)
      Monad.>=> typed @Label (parseAttribute attr)
      Monad.>=> typed @SourceLocation (parseAttribute attr)
      Monad.>=> typed @Constructor (parseAttribute attr)

  toAttributes =
    mconcat
      . sequence
          [ toAttributes . heatCool,
            toAttributes . productionID,
            toAttributes . assoc,
            toAttributes . comm,
            toAttributes . unit,
            toAttributes . idem,
            toAttributes . trusted,
            toAttributes . concrete,
            toAttributes . simplification,
            toAttributes . overload,
            toAttributes . smtLemma,
            toAttributes . label,
            toAttributes . sourceLocation,
            toAttributes . constructor
            ]
