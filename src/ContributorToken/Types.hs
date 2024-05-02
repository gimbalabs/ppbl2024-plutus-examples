{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ContributorToken.Types
  (
    ContribReferenceParams (..),
    ContributorReferenceDatum (..),
    ContributorReferenceAction (..),
    ContributorReferenceTypes,
  )
where

import GHC.Generics (Generic)
import Plutus.Script.Utils.V1.Typed.Scripts.Validators (DatumType, RedeemerType)
import Plutus.Script.Utils.V2.Typed.Scripts (TypedValidator, ValidatorTypes, mkTypedValidator, mkTypedValidatorParam, validatorScript)
import Plutus.V1.Ledger.Value
import Plutus.V2.Ledger.Api
import Plutus.V2.Ledger.Contexts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude (Show (..))
import qualified Prelude as Pr

data ContribReferenceParams = ContribReferenceParams
  { ppblCourseYear :: Integer,
    contributorTokenPolicyId :: CurrencySymbol
  }
  deriving (Pr.Eq, Pr.Ord, Show, Generic)

PlutusTx.makeLift ''ContribReferenceParams

data ContributorReferenceDatum = ContributorReferenceDatum
  { luckyNumber    :: !Integer,
    personalInfo   :: !BuiltinByteString,
    collaborator   :: !BuiltinByteString
  }

instance Eq ContributorReferenceDatum where
  {-# INLINEABLE (==) #-}
  ContributorReferenceDatum lN pI cS == ContributorReferenceDatum lN' pI' cS' = (lN == lN') && (pI == pI') && (cS == cS')

PlutusTx.unstableMakeIsData ''ContributorReferenceDatum
PlutusTx.makeLift ''ContributorReferenceDatum

data ContributorReferenceAction = UpdateNumber Integer | UpdateInfo BuiltinByteString | AddCollaborator BuiltinByteString
  deriving (Show)

PlutusTx.makeIsDataIndexed ''ContributorReferenceAction [('UpdateNumber, 0), ('UpdateInfo, 1), ('AddCollaborator, 2)]
PlutusTx.makeLift ''ContributorReferenceAction

data ContributorReferenceTypes

instance ValidatorTypes ContributorReferenceTypes where
  type DatumType ContributorReferenceTypes = ContributorReferenceDatum
  type RedeemerType ContributorReferenceTypes = ContributorReferenceAction