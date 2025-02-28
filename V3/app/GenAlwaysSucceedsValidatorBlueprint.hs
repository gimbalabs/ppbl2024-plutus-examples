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

import           AlwaysSucceedsValidator
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import           PlutusTx                    (BuiltinData)
import           PlutusTx.Blueprint
import           System.Environment          (getArgs)

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "always-succeeds-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions = deriveDefinitions @'[]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Always Succeeds Validator"
    , preambleDescription =
        Just "Blueprint for a Plutus script that always validates"
    , preambleVersion = "1.1.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Always Succeeds Validator"
    , validatorDescription =
        Just "Plutus script that always validates any transaction"
    , validatorParameters = []  -- No parameters needed
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the always succeeding validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @BuiltinData
          }
    , validatorDatum =
        Just $ MkArgumentBlueprint
          { argumentTitle = Just "Datum"
          , argumentDescription = Just "Datum for the always succeeding validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @BuiltinData
          }
    , validatorCompiled =
        Just $ compiledValidator PlutusV3 (Short.fromShort (serialiseCompiledCode alwaysSucceedsScript))
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args) 