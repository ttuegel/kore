module Kore.ASTVerifier.Verifier
    ( Verifier
    , VerifierContext (..)
    , AttributesVerification (..)
    , runVerifier
    , VerifyError
    , VerifySuccess (..)
    , verifySuccess
    , parseAttributes
    , module Kore.Error
    ) where

import Control.Monad.Except
       ( MonadError, liftEither )
import Control.Monad.Reader
       ( MonadReader, ReaderT, runReaderT )
import Data.Proxy
       ( Proxy )

import           Kore.AST.Common
import           Kore.AST.Kore
import           Kore.AST.MetaOrObject
import           Kore.AST.Sentence
import           Kore.Attribute.Parser
                 ( ParseAttributes )
import qualified Kore.Attribute.Parser as Attribute
import           Kore.Error
import           Kore.IndexedModule.IndexedModule
                 ( KoreIndexedModule )

{- | A tag for verification errors.
 -}
data VerifyError

{- | A tag for verification success.
 -}
-- TODO (thomas.tuegel): Remove VerifySuccess.
data VerifySuccess = VerifySuccess
    deriving (Eq, Show)

{-| Whether we should verify attributes and, when verifying, the module with
declarations visible in these atributes. -}
data AttributesVerification atts
    = VerifyAttributes (Proxy atts)
    | DoNotVerifyAttributes

data VerifierContext attrs =
    VerifierContext
        { indexedModule :: KoreIndexedModule attrs
        , attributesVerification :: AttributesVerification attrs
        , hookedSortVerifier
            :: KoreSentenceSort Object
            -> Verifier attrs (KoreSentenceSort Object)
        , hookedSymbolVerifier
            :: KoreSentenceSymbol Object
            -> Verifier attrs (KoreSentenceSymbol Object)
        , domainValueVerifier
            :: forall builtin.
               (builtin ~ BuiltinDomain CommonKorePattern)
            => DomainValue Object builtin
            -> Verifier attrs (DomainValue Object builtin)
        }

newtype Verifier attrs a =
    Verifier
        { getVerifier
            :: ReaderT (VerifierContext attrs)
                (Either (Error VerifyError)) a
        }
    deriving (Applicative, Functor, Monad)

deriving instance MonadError (Error VerifyError) (Verifier attrs)
deriving instance MonadReader (VerifierContext attrs) (Verifier attrs)

runVerifier :: Verifier attrs a -> VerifierContext attrs -> Either (Error VerifyError) a
runVerifier verifier ctx = runReaderT (getVerifier verifier) ctx

{- | Signal successful verification.
 -}
verifySuccess :: Verifier attrs VerifySuccess
verifySuccess = return VerifySuccess

{- | Parse attributes during verification.
 -}
parseAttributes :: ParseAttributes attrs => Attributes -> Verifier attrs attrs
parseAttributes =
    withContext "parsing attributes"
    . liftEither
    . castError
    . Attribute.parseAttributes
