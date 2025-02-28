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

-- Standard library imports
import           System.Environment          (getArgs)

-- Third party imports
import qualified Data.ByteString.Short       as Short
import qualified Data.Set                    as Set
import           PlutusLedgerApi.Common      (serialiseCompiledCode)
import qualified PlutusLedgerApi.V2          as V2
import           PlutusTx.Blueprint
import           PlutusTx.Builtins.HasOpaque (stringToBuiltinByteStringHex)

-- Local imports
import           FaucetValidator

faucetParams :: FaucetParams
faucetParams =
  FaucetParams
    {  accessTokenSymbol =
        -- Replace with your desired currency symbol (minting policy hash):
        V2.CurrencySymbol (
          stringToBuiltinByteStringHex
            "99999999999999999999999999999999999999999999999999999999"
          )
    , faucetTokenSymbol =
        -- Replace with your desired currency symbol (minting policy hash):
        V2.CurrencySymbol (
          stringToBuiltinByteStringHex
            "88888888888888888888888888888888888888888888888888888888"
          )
    }

myContractBlueprint :: ContractBlueprint
myContractBlueprint =
  MkContractBlueprint
    { contractId = Just "faucet-validator"
    , contractPreamble = myPreamble
    , contractValidators = Set.singleton myValidator
    , contractDefinitions =
        deriveDefinitions @[FaucetParams, FaucetDatum, FaucetRedeemer]
    }

myPreamble :: Preamble
myPreamble =
  MkPreamble
    { preambleTitle = "Faucet Validator"
    , preambleDescription =
        Just "Blueprint for a Plutus script validating faucet transactions"
    , preambleVersion = "1.0.0"
    , preamblePlutusVersion = PlutusV3
    , preambleLicense = Just "MIT"
    }

myValidator :: ValidatorBlueprint referencedTypes
myValidator =
  MkValidatorBlueprint
    { validatorTitle = "Faucet Validator"
    , validatorDescription =
        Just "Plutus script validating faucet transactions"
    , validatorParameters =
        [ MkParameterBlueprint
            { parameterTitle = Just "Parameters"
            , parameterDescription = Just "Compile-time validator parameters"
            , parameterPurpose = Set.singleton Spend
            , parameterSchema = definitionRef @FaucetParams
            }
        ]
    , validatorRedeemer =
        MkArgumentBlueprint
          { argumentTitle = Just "Redeemer"
          , argumentDescription = Just "Redeemer for the faucet validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @FaucetRedeemer
          }
    , validatorDatum = 
        Just $ MkArgumentBlueprint 
          { argumentTitle = Just "Datum"
          , argumentDescription = Just "Datum for the faucet validator"
          , argumentPurpose = Set.fromList [Spend]
          , argumentSchema = definitionRef @FaucetDatum
          }
    , validatorCompiled = do
        let script = faucetValidatorScript faucetParams
        let code = Short.fromShort (serialiseCompiledCode script)
        Just (compiledValidator PlutusV3 code)
    }

writeBlueprintToFile :: FilePath -> IO ()
writeBlueprintToFile path = writeBlueprint path myContractBlueprint

main :: IO ()
main =
  getArgs >>= \case
    [arg] -> writeBlueprintToFile arg
    args -> fail $ "Expects one argument, got " <> show (length args)
