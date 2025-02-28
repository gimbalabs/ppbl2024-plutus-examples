{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ImportQualifiedPost #-}
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
    , BuiltinData
    , BuiltinUnit
    , Maybe(Just)
    , check
    , ($)
    )
import PlutusLedgerApi.V3 qualified as V3
import PlutusLedgerApi.Data.V3 qualified as V3Data

typedValidator :: V3.Datum -> V3.Redeemer-> Bool
typedValidator _datum _redeemer = 
  True

untypedValidator :: BuiltinData -> BuiltinUnit
untypedValidator scriptContext =
  check
    $ case V3Data.unsafeFromBuiltinData scriptContext of
      V3Data.ScriptContext
        _txInfo
        (V3Data.Redeemer redeemer)
        (V3Data.SpendingScript _ (Just (V3Data.Datum datum))) ->
          typedValidator
            (V3.unsafeFromBuiltinData datum)
            (V3.unsafeFromBuiltinData redeemer)
      _ -> False

{-# INLINEABLE untypedValidator #-}
alwaysSucceedsScript :: CompiledCode (BuiltinData -> BuiltinUnit)
alwaysSucceedsScript =
  $$(compile [||untypedValidator||])