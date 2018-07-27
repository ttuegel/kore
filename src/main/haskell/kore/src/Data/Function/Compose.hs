{-|
Module      : Data.Function.Compose
Description : Short-hand notation for function composition with many arguments
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : traian.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Data.Function.Compose where

{-| Compose two functions where the second has two arguments.

* @(g <..> f) a1 a2 == g (f a1 a2)@
-}
(<..>) :: (b -> c) -> (a1 -> a2 -> b) -> (a1 -> a2 -> c)
(<..>) = (.) . (.)

{-| Compose two functions where the second has three arguments.

* @(g <...> f) a1 a2 a3 == g (f a1 a2 a3)@
-}
(<...>) :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> (a1 -> a2 -> a3 -> c)
(<...>) = (<..>) . (.)

{-| Compose two functions where the second has four arguments

* @(g <...> f) a1 a2 a3 a4 == g (f a1 a2 a3 a4)@
-}
(<....>) ::
       (b -> c) -> (a1 -> a2 -> a3 -> a4 -> b) -> (a1 -> a2 -> a3 -> a4 -> c)
(<....>) = (<...>) . (.)
