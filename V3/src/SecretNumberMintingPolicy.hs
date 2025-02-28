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
{-# LANGUAGE TypeApplications          #-}
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
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module SecretNumberMintingPolicy where

-- External library imports
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.Data.V3 qualified as V3Data
import PlutusTx 
    ( CompiledCode
    , compile
    )
import PlutusTx.Prelude 
    ( Bool(..)
    , BuiltinData
    , BuiltinUnit
    , Integer
    , check
    , traceIfFalse
    , ($)
    , (==)
    )


{-# INLINEABLE secretNumberTypedMintingPolicy #-}
secretNumberTypedMintingPolicy ::
  Integer ->
  Bool
secretNumberTypedMintingPolicy redeemer = 
  traceIfFalse "Redeemer Does Not Match Secret Number" (redeemer == 1618033988)

secretNumberUntypedMintingPolicy ::
  BuiltinData ->
  BuiltinUnit
secretNumberUntypedMintingPolicy scriptContext =
  check
    $ case V3Data.unsafeFromBuiltinData scriptContext of
      V3Data.ScriptContext
        _txInfo
        (V3Data.Redeemer redeemer)
        _spendingScript ->
          secretNumberTypedMintingPolicy
            (V3.unsafeFromBuiltinData redeemer)

secretNumberMintingPolicyScript ::
  CompiledCode (BuiltinData -> BuiltinUnit)
secretNumberMintingPolicyScript =
  $$(compile [||secretNumberUntypedMintingPolicy||])
