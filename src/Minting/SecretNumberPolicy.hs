{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}

module Minting.SecretNumberPolicy where

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api as PlutusV2
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Plutus.Script.Utils.Typed (mkUntypedMintingPolicy)

import Cardano.Api
import Cardano.Api.Shelley (PlutusScript (..))

import Codec.Serialise (serialise)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import qualified Plutus.V2.Ledger.Api
import Prelude (FilePath, IO)

{-# INLINABLE mkPolicy #-}
mkPolicy :: Integer -> ScriptContext -> Bool
mkPolicy _redeemer _ctx = _redeemer == 1618033988

policy :: MintingPolicy
policy = mkMintingPolicyScript $$(PlutusTx.compile [|| wrap ||])
 where
  wrap = mkUntypedMintingPolicy mkPolicy

plutusScript :: PlutusV2.Script
plutusScript = unMintingPolicyScript policy

validator :: Validator
validator = Validator plutusScript

writeValidator :: FilePath -> Plutus.V2.Ledger.Api.Validator -> IO (Either (FileError ()) ())
writeValidator file = writeFileTextEnvelope @(PlutusScript PlutusScriptV2) file Nothing . PlutusScriptSerialised . SBS.toShort . LBS.toStrict . serialise . Plutus.V2.Ledger.Api.unValidatorScript

writePlutusMintingScript :: IO (Either (FileError ()) ())
writePlutusMintingScript = writeValidator "output/mint-secret-number.plutus" validator
