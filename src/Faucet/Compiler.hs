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
    { accessTokenSymbol = "903c419ee7ebb6bf4687c61fb133d233ef9db2f80e4d734db3fbaf0b",
      faucetTokenSymbol = "5e74a87d8109db21fe3d407950c161cd2df7975f0868e10682a3dbfe"
    }

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writeFaucetValidatorScript :: IO (Either (FileError ()) ())
writeFaucetValidatorScript = writeValidator "output/my-faucet-script.plutus" $ Validator.validator faucetParams
