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

module Faucet.Validator where

import GHC.Generics (Generic)
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidatorParam, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show)

import Faucet.Types

{-# INLINEABLE treasuryValidator #-}
treasuryValidator :: FaucetParams -> FaucetDatum -> FaucetRedeemer -> ScriptContext -> Bool
treasuryValidator fp datum redeemer ctx =
  let info :: TxInfo
      info = scriptContextTxInfo ctx

      allTokens :: [CurrencySymbol]
      allTokens = symbols $ valueSpent info

      inputHasAccessToken :: Bool
      inputHasAccessToken = (accessTokenSymbol fp) `elem` allTokens

      valueToReceiver :: Value
      valueToReceiver = valuePaidTo info (senderPkh redeemer)

      outputHasAccessToken :: Bool
      outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol fp) (accessTokenName redeemer)) >= 1

      outputHasFaucetToken :: Bool
      outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol fp) (faucetTokenName datum)) == (withdrawalAmount datum)

      ownInput :: TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i -> txInInfoResolved i

      -- What edge cases can you think of?
      ownOutput :: TxOut
      ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _ -> traceError "expected exactly one faucet output"

      faucetInputValue :: Value
      faucetInputValue = txOutValue ownInput

      faucetOutputValue :: Value
      faucetOutputValue = txOutValue ownOutput

      faucetContractGetsRemainingTokens :: Bool
      faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol fp) (faucetTokenName datum) - (withdrawalAmount datum) <= (valueOf faucetOutputValue (faucetTokenSymbol fp) (faucetTokenName datum)))

      getDatum :: Maybe FaucetDatum
      getDatum = case txOutDatum ownOutput of
        (OutputDatum (Datum d)) -> case fromBuiltinData d of
          Nothing -> Nothing
          Just mrd -> Just $ unsafeFromBuiltinData @FaucetDatum mrd
        _ -> Nothing

      checkDatum :: Bool
      checkDatum = case getDatum of
        Nothing -> False
        Just d -> d == datum
   in traceIfFalse "Input must include PPBL 2023 token" inputHasAccessToken
        && traceIfFalse "Must send PPBL 2023 token back to sender" outputHasAccessToken
        && traceIfFalse "Sender must receive faucet tokens" outputHasFaucetToken
        && traceIfFalse "Must return remaining faucet tokens to contract address" faucetContractGetsRemainingTokens
        && traceIfFalse "Cannot change datum" checkDatum

data FaucetTypes

instance ValidatorTypes FaucetTypes where
  type DatumType FaucetTypes = FaucetDatum
  type RedeemerType FaucetTypes = FaucetRedeemer

typedValidator :: FaucetParams -> TypedValidator FaucetTypes
typedValidator fp = go fp
  where
    go =
      mkTypedValidatorParam @FaucetTypes
        $$(PlutusTx.compile [||treasuryValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: FaucetParams -> Validator
validator = validatorScript . typedValidator
