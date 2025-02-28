{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE Strict                     #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_GHC -fno-full-laziness #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
{-# OPTIONS_GHC -fno-omit-interface-pragmas #-}
{-# OPTIONS_GHC -fno-spec-constr #-}
{-# OPTIONS_GHC -fno-specialise #-}
{-# OPTIONS_GHC -fno-strictness #-}
{-# OPTIONS_GHC -fno-unbox-small-strict-fields #-}
{-# OPTIONS_GHC -fno-unbox-strict-fields #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module FaucetValidator where

import GHC.Generics (Generic)

import PlutusCore.Version (plcVersion110)

-- Plutus Ledger API imports
import PlutusLedgerApi.V1
    ( symbols
    , valueOf
    )

import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusLedgerApi.V3.Contexts
    ( getContinuingOutputs
    , findOwnInput
    , scriptContextTxInfo
    , valuePaidTo
    , valueSpent
    )

-- PlutusTx imports
import PlutusTx
    ( CompiledCode
    , compile
    , liftCode
    , makeIsDataSchemaIndexed
    , makeLift
    , unsafeApplyCode
    )
import PlutusTx.Blueprint
    ( HasBlueprintDefinition
    , definitionRef
    )
import PlutusTx.Prelude
    ( Bool(..)
    , BuiltinData
    , BuiltinUnit
    , Eq
    , Integer
    , Maybe(Just, Nothing)
    , check
    , elem
    , traceError
    , traceIfFalse
    , ($)
    , (&&)
    , (==)
    , (>=)
    , (-)
    )

data FaucetParams = FaucetParams
  { accessTokenSymbol :: !V3.CurrencySymbol,
    faucetTokenSymbol :: !V3.CurrencySymbol
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeLift ''FaucetParams
makeIsDataSchemaIndexed ''FaucetParams [('FaucetParams, 0)]

data FaucetDatum = FaucetDatum
  { withdrawalAmount :: !Integer,
    faucetTokenName :: !V3.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

instance Eq FaucetDatum where
  {-# INLINEABLE (==) #-}
  FaucetDatum a b == FaucetDatum a' b' = (a == a') && (b == b')

makeIsDataSchemaIndexed ''FaucetDatum [('FaucetDatum, 0)]

data FaucetRedeemer = FaucetRedeemer
  { senderPkh :: !V3.PubKeyHash,
    accessTokenName :: !V3.TokenName
  }
  deriving stock (Generic)
  deriving anyclass (HasBlueprintDefinition)

makeIsDataSchemaIndexed ''FaucetRedeemer [('FaucetRedeemer, 0)]


{-# INLINEABLE faucetTypedValidator #-}

faucetTypedValidator :: FaucetParams -> FaucetDatum -> FaucetRedeemer -> V3.ScriptContext -> Bool
faucetTypedValidator fp datum redeemer ctx =
  let info :: V3.TxInfo
      info = scriptContextTxInfo ctx

      allTokens :: [V3.CurrencySymbol]
      allTokens = symbols $ valueSpent info

      inputHasAccessToken :: Bool
      inputHasAccessToken = (accessTokenSymbol fp) `elem` allTokens

      valueToReceiver :: V3.Value
      valueToReceiver = valuePaidTo info (senderPkh redeemer)

      outputHasAccessToken :: Bool
      outputHasAccessToken = (valueOf valueToReceiver (accessTokenSymbol fp) (accessTokenName redeemer)) >= 1

      outputHasFaucetToken :: Bool
      outputHasFaucetToken = (valueOf valueToReceiver (faucetTokenSymbol fp) (faucetTokenName datum)) >= (withdrawalAmount datum)

      ownInput :: V3.TxOut
      ownInput = case findOwnInput ctx of
        Nothing -> traceError "faucet input missing"
        Just i -> V3.txInInfoResolved i

      -- What edge cases can you think of?
      ownOutput :: V3.TxOut
      ownOutput = case getContinuingOutputs ctx of
        [o] -> o
        _ -> traceError "expected exactly one faucet output"

      faucetInputValue :: V3.Value
      faucetInputValue = V3.txOutValue ownInput

      faucetOutputValue :: V3.Value
      faucetOutputValue = V3.txOutValue ownOutput

      faucetContractGetsRemainingTokens :: Bool
      faucetContractGetsRemainingTokens = (valueOf faucetInputValue (faucetTokenSymbol fp) (faucetTokenName datum) - (withdrawalAmount datum) == (valueOf faucetOutputValue (faucetTokenSymbol fp) (faucetTokenName datum)))

      getDatum :: Maybe FaucetDatum
      getDatum = case V3.txOutDatum ownOutput of
        (V3.OutputDatum (V3.Datum d)) -> case V3.fromBuiltinData d of
          Nothing -> Nothing
          Just mrd -> Just $ V3.unsafeFromBuiltinData @FaucetDatum mrd
        _ -> Nothing

      checkDatum :: Bool
      checkDatum = case getDatum of
        Nothing -> False
        Just d -> d == datum

  in traceIfFalse "Input must include PPBL 2024 token" inputHasAccessToken
        && traceIfFalse "Must send PPBL 2024 token back to sender" outputHasAccessToken
        && traceIfFalse "Sender must receive faucet tokens" outputHasFaucetToken
        && traceIfFalse "Must return remaining faucet tokens to contract address" faucetContractGetsRemainingTokens
        && traceIfFalse "Cannot change datum" checkDatum


faucetUntypedValidator :: FaucetParams -> BuiltinData -> BuiltinUnit
faucetUntypedValidator params ctx =
  check (faucetTypedValidator params faucetDatum faucetRedeemer (V3.unsafeFromBuiltinData ctx))
  where
    scriptContext :: V3Data.ScriptContext
    scriptContext = V3Data.unsafeFromBuiltinData ctx

    faucetDatum :: FaucetDatum
    faucetDatum =
      case V3Data.scriptContextScriptInfo scriptContext of
        V3Data.SpendingScript _TxOutRef (Just (V3Data.Datum datum)) -> V3.unsafeFromBuiltinData datum
        _ -> traceError "Expected SpendingScript with a datum"

    faucetRedeemer :: FaucetRedeemer
    faucetRedeemer =
      V3.unsafeFromBuiltinData (V3Data.getRedeemer (V3Data.scriptContextRedeemer scriptContext))

{-# INLINEABLE faucetUntypedValidator #-}

faucetValidatorScript :: FaucetParams -> CompiledCode (BuiltinData -> BuiltinUnit)
faucetValidatorScript params =
  $$(compile [||faucetUntypedValidator||])
    `unsafeApplyCode` liftCode plcVersion110 params
