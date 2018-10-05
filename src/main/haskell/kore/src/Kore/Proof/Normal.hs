{- |
Module      : Kore.Proof.Normal
Description : Proof of normalization of term heads
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : thomas.tuegel@runtimeverification.com
 -}

module Kore.Proof.Normal
    ( Normal (..)
    , fromConstructor
    , fromNormal
    , asNormalConcreteTerm
    ) where

import qualified Data.Functor.Foldable as Functor.Foldable
import           Data.Reflection
                 ( give )

import Kore.AST.Common
import Kore.IndexedModule.MetadataTools
import Kore.Step.StepperAttributes

{- | A simple normalization proof.

    A normal pattern head is either a constructor (or a constructor-like domain
    value) or a sort injection.
 -}
data Normal a
    = Constructor a
    | SortInjection a
    | NotNormal
    deriving (Eq, Foldable, Functor, Traversable)

{- | Return the normal value if it is a constructor.
 -}
fromConstructor :: Normal a -> Maybe a
fromConstructor =
    \case
        Constructor a -> Just a
        _ -> Nothing

{- | Return any normal value.
 -}
fromNormal :: Normal a -> Maybe a
fromNormal =
    \case
        Constructor a -> Just a
        SortInjection a -> Just a
        NotNormal -> Nothing

{- | Return the normalized pattern head, if its children are normal.

    The primary complication is that sort injection heads are considered normal,
    but only if their immediate children are not also sort injections; otherwise
    the term could be simplified by applying the triangle axiom.

    See also: 'asNormalConcreteTerm'

 -}
asNormalPattern
    :: MetadataTools level StepperAttributes
    -> Pattern level Concrete (Normal child)
    -> Normal (Pattern level Concrete child)
asNormalPattern tools =
    \case
        ApplicationPattern
            appP@Application
                { applicationSymbolOrAlias = symbolOrAlias
                , applicationChildren
                }
          | isConstructor symbolOrAlias ->
            -- The constructor application is normal if all its children are
            -- normal.
            maybe NotNormal (Constructor . ApplicationPattern)
                (do
                    children <- normalChildren
                    return appP { applicationChildren = children }
                )
          | isSortInjection symbolOrAlias ->
            -- The sort injection application is normal if all its children are
            -- normal and none are sort injections.
            maybe NotNormal (SortInjection . ApplicationPattern)
                (do
                    children <- constructorChildren
                    return appP { applicationChildren = children }
                )
          where
            constructorChildren = traverse fromConstructor applicationChildren
            normalChildren = traverse fromNormal applicationChildren
        DomainValuePattern dvP ->
            -- A domain value is not technically a constructor, but it is
            -- constructor-like for builtin domains, at least from the
            -- perspective of normalization.
            -- TODO (thomas.tuegel): Builtin domain parsers may violate the
            -- assumption that domain values are concrete. We should remove
            -- BuiltinDomainPattern and always run the stepper with internal
            -- representations only.
            maybe NotNormal (Constructor . DomainValuePattern)
                (traverse (traverse fromNormal) dvP)
        _ -> NotNormal
  where
    isConstructor = give tools isConstructor_
    isSortInjection = give tools isSortInjection_

{- | View a 'ConcretePurePattern' as a normalized term.

    The pattern is considered normalized if it is a domain value, a constructor,
    or a sort injection applied only to normalized terms.

    See also: 'asNormalPattern'

 -}
asNormalConcreteTerm
    :: forall level.
       MetadataTools level StepperAttributes
    -> ConcretePurePattern level
    -> Maybe (ConcretePurePattern level)
asNormalConcreteTerm tools =
    fromNormal . Functor.Foldable.fold asNormalConcreteTerm1
  where
    asNormalConcreteTerm1 =
        fmap Functor.Foldable.embed . asNormalPattern tools
