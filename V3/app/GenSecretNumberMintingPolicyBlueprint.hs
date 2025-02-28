{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           SecretNumberMintingPolicy
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import           PlutusTx.Blueprint
import           System.Environment          (getArgs)

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "secret-number-minting-policy"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @'[Integer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Secret Number Minting Policy"
    , preambleDescription =
        Just "Blueprint for a Minting Policy that checks for a secret number"
    , preambleVersion = "1.1.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Secret Number Minting Policy Validator"
    , validatorDescription =
        Just "Plutus script mints a token if the redeemer is the secret number"
    , validatorParameters = []  -- No parameters needed
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Secret Number Redeemer"
          , argumentDescription = Just "The secret number to check for"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @Integer
          }
    , validatorDatum = Nothing
    , validatorCompiled =
        Just $ compiledValidator PlutusV3 (Short.fromShort (serialiseCompiledCode secretNumberMintingPolicyScript))
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args) 