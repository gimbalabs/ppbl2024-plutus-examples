{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE NumericUnderscores  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:target-version=1.1.0 #-}
--{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:remove-trace #-}
{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:preserve-logging #-}

module AlwaysSucceedsValidator where

import PlutusTx (CompiledCode, compile)
import PlutusTx.Prelude
    ( Bool
    , Bool(..)
    , BuiltinUnit
    , Maybe(Just)
    , check
    , traceError
    , ($)
    )
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.Data.V3 qualified as V3Data

typedValidator :: V3.Datum -> V3.Redeemer-> Bool
typedValidator _datum _redeemer = 
  True

untypedValidator :: 
  V3Data.ScriptContext -> 
  BuiltinUnit
untypedValidator ctx =
  check $ typedValidator getDatum (getRedeemer ctx) 
  where
    getRedeemer :: V3Data.ScriptContext -> V3Data.Redeemer
    getRedeemer V3Data.ScriptContext {V3Data.scriptContextRedeemer} = scriptContextRedeemer

    getDatum :: V3Data.Datum
    getDatum =
      case V3Data.scriptContextScriptInfo ctx of
        V3Data.SpendingScript _TxOutRef (Just (V3Data.Datum datum)) -> V3.unsafeFromBuiltinData datum
        _ -> traceError "No datum found"


{-# INLINEABLE untypedValidator #-}
alwaysSucceedsScript :: CompiledCode (V3Data.ScriptContext -> BuiltinUnit)
alwaysSucceedsScript =
  $$(compile [||untypedValidator||])