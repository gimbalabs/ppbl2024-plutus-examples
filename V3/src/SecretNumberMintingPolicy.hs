{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ImportQualifiedPost        #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
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
  V3Data.ScriptContext ->
  BuiltinUnit
secretNumberUntypedMintingPolicy ctx =
  check $ secretNumberTypedMintingPolicy (getRedeemer ctx)
  where
    getRedeemer :: V3Data.ScriptContext -> Integer
    getRedeemer V3Data.ScriptContext {V3Data.scriptContextRedeemer = V3Data.Redeemer redeemer} = 
      V3Data.unsafeFromBuiltinData redeemer

secretNumberMintingPolicyScript ::
  CompiledCode (V3Data.ScriptContext -> BuiltinUnit)
secretNumberMintingPolicyScript =
  $$(compile [||secretNumberUntypedMintingPolicy||])
