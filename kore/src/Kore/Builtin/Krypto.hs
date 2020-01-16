{- |
Module      : Kore.Builtin.Krypto
Description : Built-in cryptographic functions.
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
Maintainer  : vladimir.ciobanu@runtimeverification.com
Stability   : experimental
Portability : portable

This module is intended to be imported qualified, to avoid collision with other
builtin modules.

@
    import qualified Kore.Builtin.Krypto as Krypto
@
 -}
module Kore.Builtin.Krypto
    ( verifiers
    , builtinFunctions
    , signatureToKey
    -- * Constants
    , keccak256Key
    , sha256Key
    , sha3256Key
    , ripemd160Key
    , ecdsaRecoverKey
    ) where


import Control.Exception.Base
    ( assert
    )
import Crypto.Hash
    ( HashAlgorithm
    , Keccak_256 (..)
    , RIPEMD160 (..)
    , SHA256 (..)
    , SHA3_256 (..)
    , hashWith
    )
import Crypto.PubKey.ECC.Prim
import Crypto.PubKey.ECC.Types
import Data.Bits
import Data.ByteString
    ( ByteString
    )
import qualified Data.ByteString as ByteString
import Data.Char as Char
import qualified Data.HashMap.Strict as HashMap
import Data.Map.Strict
    ( Map
    )
import qualified Data.Map.Strict as Map
import Data.String
    ( IsString
    , fromString
    )
import Data.Text
    ( Text
    )
import qualified Data.Text as Text
import Data.Word
    ( Word8
    )
import GHC.Stack
    ( HasCallStack
    )
import Kore.Builtin.Encoding
    ( encode8Bit
    )

import qualified Kore.Builtin.Builtin as Builtin
import qualified Kore.Builtin.Int as Int
import qualified Kore.Builtin.String as String

keccak256Key, ecdsaRecoverKey, sha256Key, sha3256Key, ripemd160Key
    :: IsString s => s
keccak256Key = "KRYPTO.keccak256"
ecdsaRecoverKey = "KRYPTO.ecdsaRecover"
sha256Key = "KRYPTO.sha256"
sha3256Key = "KRYPTO.sha3256"
ripemd160Key = "KRYPTO.ripemd160"

verifiers :: Builtin.Verifiers
verifiers =
    Builtin.Verifiers
        { sortDeclVerifiers = mempty
        , symbolVerifiers
        , patternVerifierHook = mempty
        }

{- | Verify that hooked symbol declarations are well-formed.

  See also: 'Builtin.verifySymbol'

-}
symbolVerifiers :: Builtin.SymbolVerifiers
symbolVerifiers =
    HashMap.fromList
    [ (keccak256Key, verifyHashFunction)
    , (sha256Key, verifyHashFunction)
    , (sha3256Key, verifyHashFunction)
    , (ripemd160Key, verifyHashFunction)
    , (ecdsaRecoverKey
      , Builtin.verifySymbol
            String.assertSort
            [ String.assertSort
            , Int.assertSort
            , String.assertSort
            , String.assertSort
            ]
      )
    ]

{- | Implement builtin function evaluation.
 -}
builtinFunctions :: Map Text Builtin.Function
builtinFunctions =
    Map.fromList
        [ (keccak256Key, evalKeccak)
        , (sha256Key, evalSha256)
        , (sha3256Key, evalSha3256)
        , (ripemd160Key, evalRipemd160)
        , (ecdsaRecoverKey, evalECDSARecover)
        ]

verifyHashFunction :: Builtin.SymbolVerifier
verifyHashFunction = Builtin.verifySymbol String.assertSort [String.assertSort]

{- | A function evaluator for builtin hash function hooks.

The symbol's argument must be a string which will be interpreted as a sequence
of 8-bit bytes. The result is the hash as a string in big-endian base-16
encoding.

 -}
evalHashFunction
    :: HashAlgorithm algorithm
    => String  -- ^ hook name for error messages
    -> algorithm  -- ^ hash function
    -> Builtin.Function
evalHashFunction context algorithm =
    Builtin.functionEvaluator evalHashFunctionWorker
  where
    evalHashFunctionWorker :: Builtin.FunctionImplementation
    evalHashFunctionWorker resultSort arguments =
        Builtin.getAttemptedAxiom $ do
            let
                arg =
                    case arguments of
                      [input] -> input
                      _ -> Builtin.wrongArity context
            str <- String.expectBuiltinString context arg
            let bytes = encode8Bit str
                digest = hashWith algorithm bytes
                result = fromString (show digest)
            Builtin.appliedFunction $ String.asPattern resultSort result

evalKeccak :: Builtin.Function
evalKeccak = evalHashFunction keccak256Key Keccak_256

evalSha256 :: Builtin.Function
evalSha256 = evalHashFunction sha256Key SHA256

evalSha3256 :: Builtin.Function
evalSha3256 = evalHashFunction sha3256Key SHA3_256

evalRipemd160 :: Builtin.Function
evalRipemd160 = evalHashFunction ripemd160Key RIPEMD160

evalECDSARecover :: Builtin.Function
evalECDSARecover =
    Builtin.functionEvaluator eval0
  where
    eval0 :: Builtin.FunctionImplementation
    eval0 resultSort [messageHash0, v0, r0, s0] =
        Builtin.getAttemptedAxiom $ do
            messageHash <- string2Integer . Text.unpack
                    <$> String.expectBuiltinString "" messageHash0
            v <- Int.expectBuiltinInt "" v0
            r <- string2Integer . Text.unpack
                    <$> String.expectBuiltinString "" r0
            s <- string2Integer . Text.unpack
                    <$> String.expectBuiltinString "" s0
            Builtin.appliedFunction
                $ String.asPattern resultSort
                $ Text.pack
                $ byteString2String
                $ pad 64 0
                $ signatureToKey messageHash r s v
    eval0 _ _ = Builtin.wrongArity ecdsaRecoverKey

pad :: Int -> Word8 -> ByteString -> ByteString
pad n w s = ByteString.append s padding
  where
    padding =
        ByteString.replicate (n - ByteString.length s) w


signatureToKey
    :: Integer
    -> Integer
    -> Integer
    -> Integer
    -> ByteString
signatureToKey messageHash r s v =
      assert (27 <= v && v <= 34)
    $ ByteString.drop 1
    $ encodePoint compressed
    $ recoverPublicKey recId (r, s) messageHash
  where
    recId = v - 27
    compressed = v >= 31

recoverPublicKey
    :: Integer
    -> (Integer, Integer)
    -> Integer
    -> Point
recoverPublicKey recId (r, s) e =
      assert (recId >= 0)
    $ assert (r >= 0)
    $ assert (s >= 0)
    $ assert (pt_x <= p)
    $ assert (pointMul p256k1 n pt == PointO)
    $ pointAddTwoMuls
            p256k1
            (mulMod n (invMod n r) s)
            pt
            (mulMod n (invMod n r) (n - e `mod` n))
            (ecc_g curveParams)
  where
    p256k1 = getCurveByName SEC_p256k1
    CurvePrime p curveParams =
        case p256k1 of
            CurveFP curvePrime@(CurvePrime _ _) -> curvePrime
            _ -> error "Expected CurveFP!"

    n = ecc_n curveParams

    i = recId `div` 2

    pt_x = r + i*n

    pt = decompressPt pt_x (recId .&. 1 == 1)

    decompressPt x signBit = Point x (if signBit /= even y then y else p - y)
      where
        y = sqrtMod p
            ( powMod p x 3
            + mulMod p (ecc_a curveParams) x
            + ecc_b curveParams
            )

invMod
    :: Integer
    -> Integer
    -> Integer
invMod p x = powMod p x (p - 2)

sqrtMod
    :: Integer
    -> Integer
    -> Integer
sqrtMod p x = powMod p x ((p + 1) `div` 4)

mulMod
    :: Integer
    -> Integer
    -> Integer
    -> Integer
mulMod p x y = (x * y) `mod` p

powMod
    :: Integer
    -> Integer
    -> Integer
    -> Integer
powMod _ _ 0 = 1
powMod p x a =
    mulMod p
    (if even a then 1 else x)
    (powMod p (mulMod p x x) (a `div` 2))

-- Leading byte signals whether the point is compressed.
-- Superfluous because we drop it later on.
-- Kept here for completeness sake, to match
-- the code in the java backend.
encodePoint
    :: HasCallStack
    => Bool
    -> Point
    -> ByteString
encodePoint compressed (Point x y)
  | compressed =
    ByteString.cons
        (if even y then 0x02 else 0x03)
        (integer2ByteString x)
  | otherwise =
    ByteString.concat
        [ ByteString.pack [0x04]
        , integer2ByteString x
        , integer2ByteString y
        ]
encodePoint _ _ = error "Should never obtain point-at-infinity here!"

{- | Converts a 'String' to a 'ByteString'.

Will error if the string contains any characters above @\255@.

 -}
byteString2String :: ByteString -> String
byteString2String = map (chr . fromIntegral) . ByteString.unpack

-- | Interprets a 'String' as an 'Integer' in big-endian unsigned
-- representation.
string2Integer :: String -> Integer
string2Integer = bstring2Integer . ByteString.pack . map (fromIntegral . ord)

-- | Interprets a 'ByteString' as an 'Integer' in big-endian unsigned
-- representation.
bstring2Integer :: ByteString -> Integer
bstring2Integer =
    ByteString.foldr (\word i -> fromIntegral word + (i `shiftL` 8)) 0
  . ByteString.reverse

-- | Converts an Integer to its big-endian unsigned representation.
integer2ByteString :: Integer -> ByteString
integer2ByteString =
    ByteString.reverse .  ByteString.unfoldr integer2ByteStringWorker
  where
    integer2ByteStringWorker i
      | i == 0 = Nothing
      | otherwise = Just (fromIntegral r, q)
      where
        (q, r) = divMod i 256
