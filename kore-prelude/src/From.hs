{- |
Copyright   : (c) Runtime Verification, 2020
License     : NCSA

 -}

module From
    ( From (..)
    ) where

{- | Convert type @from@ into @to@.

Valid instances are /total/. @from@ should be a homomorphism
(structure-preserving map); what structure is preserved shall be determined by
the implementer.

Usage with @TypeApplications@:

@
let b = let a = _ in from @A @B a
@

Usage with both types inferred:

@
let b :: B = let a :: A = _ in from a
@

Usage with only @to@ inferred:

@
let b :: B = let a = _ in from @A a
@

Usage with only @from@ inferred:

@
let b = let a :: A = _ in from @_ @B a
@
 -}
class From from to where
    from :: from -> to
