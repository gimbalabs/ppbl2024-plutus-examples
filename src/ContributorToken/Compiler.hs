{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ContributorToken.Compiler where

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV2)
import Codec.Serialise (serialise)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V1.Ledger.Scripts
import qualified Plutus.V1.Ledger.Value
import qualified Plutus.V2.Ledger.Api
import qualified Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import qualified ContributorToken.Validator as ContributorToken
import ContributorToken.Types

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

-- One ContribTokenValidator is all we need
contribReferenceParams :: ContribReferenceParams
contribReferenceParams = ContribReferenceParams {
    ppblCourseYear = 2024,
    contributorTokenPolicyId = "903c419ee7ebb6bf4687c61fb133d233ef9db2f80e4d734db3fbaf0b"
}

writeContribReferenceTokenScript :: IO (Either (FileError ()) ())
writeContribReferenceTokenScript = writeValidator "output/ppbl-contrib-token-validator.plutus" $ ContributorToken.validator contribReferenceParams
