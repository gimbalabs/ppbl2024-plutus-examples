{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Faucet.Types where

import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)

data FaucetParams = FaucetParams
  { accessTokenSymbol :: !CurrencySymbol,
    faucetTokenSymbol :: !CurrencySymbol
  }

PlutusTx.makeLift ''FaucetParams

data FaucetDatum = FaucetDatum
  { withdrawalAmount :: !Integer,
    faucetTokenName :: !TokenName
  }

instance Eq FaucetDatum where
  {-# INLINEABLE (==) #-}
  FaucetDatum a b == FaucetDatum a' b' = (a == a') && (b == b')

PlutusTx.unstableMakeIsData ''FaucetDatum
PlutusTx.makeLift ''FaucetDatum

data FaucetRedeemer = FaucetRedeemer
  { senderPkh :: !PubKeyHash,
    accessTokenName :: !TokenName
  }

PlutusTx.unstableMakeIsData ''FaucetRedeemer
PlutusTx.makeLift ''FaucetRedeemer
