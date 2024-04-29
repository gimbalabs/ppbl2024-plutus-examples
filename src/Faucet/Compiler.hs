{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Faucet.Compiler (writeFaucetValidatorScript) where

import Cardano.Api
import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Cardano.Api.Shelley (PlutusScript (..))
import PlutusTx.Prelude
import Prelude (FilePath, IO)

import Faucet.Types
import Faucet.Validator as Validator

faucetParams :: FaucetParams
faucetParams =
  FaucetParams
    { accessTokenSymbol = "05cf1f9c1e4cdcb6702ed2c978d55beff5e178b206b4ec7935d5e056",
      faucetTokenSymbol = "a4af431031b91e9130aa6b920c3b8b5c18befeb79e9e16d473205396"
    }

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeFaucetValidatorScript :: IO (Either (FileError ()) ())
writeFaucetValidatorScript = writeValidator "output/my-faucet-script.plutus" $ Validator.validator faucetParams
