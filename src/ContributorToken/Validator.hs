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

module ContributorToken.Validator (validator) where

import Ledger (scriptHashAddress)
import Plutus.Script.Utils.Typed (mkUntypedValidator)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, validatorHash, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import PlutusTx
import PlutusTx.Builtins
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr

import ContributorToken.Types

{-# INLINEABLE contributorReferenceValidator #-}
contributorReferenceValidator :: ContribReferenceParams -> ContributorReferenceDatum -> ContributorReferenceAction -> ScriptContext -> Bool
contributorReferenceValidator param datum action ctx@ScriptContext{ scriptContextTxInfo = info@TxInfo{..}} =
  let
    inVals :: [CurrencySymbol]
    inVals = symbols $ valueSpent info

    ownOutput :: Maybe TxOut
    ownOutput = case getContinuingOutputs ctx of
      [o] -> Just o
      _   -> Nothing

    newReferenceDatum :: Maybe ContributorReferenceDatum
    newReferenceDatum = case ownOutput of
      Nothing -> Nothing
      Just o -> case txOutDatum o of
        ( OutputDatum ( Datum d)) -> case fromBuiltinData d of
          Nothing   -> Nothing
          Just mrd  -> Just $ unsafeFromBuiltinData @ContributorReferenceDatum mrd
        _                         -> Nothing

    allInputValues :: Value
    allInputValues = valueSpent info

    inputHasContributorRefTokenPair :: Bool
    inputHasContributorRefTokenPair = case [ tn | (cs, tn, _) <- flattenValue allInputValues, cs == contributorTokenPolicyId param ] of
      [a,t] -> dropByteString 3 (unTokenName a) == dropByteString 3 (unTokenName t)
      _     -> False

  in case action of
    (UpdateNumber num) ->
      let
        requiredDatum = ContributorReferenceDatum {
          luckyNumber = num,
          personalInfo = personalInfo datum,
          collaborator = collaborator datum
        }

        checkUpdatedDatum :: Bool
        checkUpdatedDatum = case newReferenceDatum of
          Nothing -> False
          Just d -> d == requiredDatum

      in
        traceIfFalse "Contributor and reference tokens do not match"    inputHasContributorRefTokenPair &&
        traceIfFalse "New datum is not valid"                           checkUpdatedDatum

    (UpdateInfo newInfo) ->
      let
        requiredDatum = ContributorReferenceDatum {
          luckyNumber = luckyNumber datum,
          personalInfo = newInfo,
          collaborator = collaborator datum
        }

        checkUpdatedDatum :: Bool
        checkUpdatedDatum = case newReferenceDatum of
          Nothing -> False
          Just d -> d == requiredDatum

      in
        traceIfFalse "Contributor and reference tokens do not match"    inputHasContributorRefTokenPair &&
        traceIfFalse "New datum is not valid"                           checkUpdatedDatum

    AddCollaborator collabPkh ->
      let
        requiredDatum = ContributorReferenceDatum {
          luckyNumber = luckyNumber datum,
          personalInfo = personalInfo datum,
          collaborator = collabPkh
        }

        checkUpdatedDatum :: Bool
        checkUpdatedDatum = case newReferenceDatum of
          Nothing -> False
          Just d -> d == requiredDatum

      in
        traceIfFalse "New datum is not valid"                           checkUpdatedDatum

typedValidator :: ContribReferenceParams -> TypedValidator ContributorReferenceTypes
typedValidator tp = go tp
  where
    go =
      mkTypedValidatorParam @ContributorReferenceTypes
        $$(PlutusTx.compile [||contributorReferenceValidator||])
        $$(PlutusTx.compile [||wrap||])
    wrap = mkUntypedValidator

validator :: ContribReferenceParams -> Validator
validator = validatorScript . typedValidator

contributorReferenceValidatorHash :: ContribReferenceParams -> ValidatorHash
contributorReferenceValidatorHash = validatorHash . typedValidator
