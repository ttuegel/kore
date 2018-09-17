module Kore.ASTVerifier.Verifier
    ( Verifier
    , runVerifier
    , VerifyError
    , VerifySuccess (..)
    , verifySuccess
    , parseAttributes
    , module Kore.Error
    ) where

import Control.Monad.Except
       ( MonadError, liftEither )

import           Kore.AST.Sentence
                 ( Attributes )
import           Kore.Attribute.Parser
                 ( ParseAttributes )
import qualified Kore.Attribute.Parser as Attribute
import           Kore.Error

{- | A tag for verification errors.
 -}
data VerifyError

{- | A tag for verification success.
 -}
-- TODO (thomas.tuegel): Remove VerifySuccess.
data VerifySuccess = VerifySuccess
    deriving (Eq, Show)

newtype Verifier a = Verifier { getVerifier :: Either (Error VerifyError) a }
    deriving (Applicative, Functor, Monad)

deriving instance MonadError (Error VerifyError) Verifier

runVerifier :: Verifier a -> Either (Error VerifyError) a
runVerifier = getVerifier

{- | Signal successful verification.
 -}
verifySuccess :: Verifier VerifySuccess
verifySuccess = return VerifySuccess

{- | Parse attributes during verification.
 -}
parseAttributes :: ParseAttributes attrs => Attributes -> Verifier attrs
parseAttributes =
    withContext "parsing attributes"
    . liftEither
    . castError
    . Attribute.parseAttributes
