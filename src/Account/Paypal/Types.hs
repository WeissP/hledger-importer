{-# OPTIONS_GHC -Wno-orphans #-}

module Account.Paypal.Types where

import Account.Downloadable (HasRecordDownloadInfo (..), RecordDownloadInfo (..))
import Data.Aeson (FromJSON (..), ToJSON, withObject, (.:))
import Data.Aeson qualified as J
import Data.Aeson.Lens qualified as J
import Data.Aeson.Types (ToJSON (..))
import Data.Pass
import MyPrelude
import RIO.Partial qualified as RIO'
import RIO.Time (Day)
import Types

paypalAccAlias :: AccAlias
paypalAccAlias = "Paypal"

newtype PaypalToken = PaypalToken ByteString
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)
makeClassy ''PaypalToken

data PaypalDownloadState = PaypalDownloadState
  { _pdsStart :: Day
  , _pdsEnd :: Day
  , _pdsPage :: Int
  }
  deriving stock (Generic)
makeClassy ''PaypalDownloadState
instance Default PaypalDownloadState where
  def = PaypalDownloadState (RIO'.toEnum 0) (RIO'.toEnum 0) 0

data PaypalEnv = PaypalEnv
  { _ppPass :: PassRecord
  , _ppToken :: PaypalToken
  , _ppRecordDownloadInfo :: RecordDownloadInfo
  , _ppState :: SomeRef PaypalDownloadState
  }
  deriving stock (Generic)
makeClassy ''PaypalEnv
instance HasRecordDownloadInfo PaypalEnv where
  recordDownloadInfo = ppRecordDownloadInfo
instance HasPaypalToken PaypalEnv where
  paypalToken = ppToken
instance (HasPaypalToken env) => (HasPaypalToken (WithLogFunc env)) where
  paypalToken = wEnv . paypalToken
instance HasStateRef PaypalDownloadState PaypalEnv where
  stateRefL = ppState
instance
  (HasStateRef PaypalDownloadState env) =>
  (HasStateRef PaypalDownloadState (WithLogFunc env))
  where
  stateRefL = wEnv . stateRefL

newtype PaypalRecord = PaypalRecord J.Value
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
instance Display PaypalRecord where
  textDisplay (PaypalRecord v) = tshow v

data PaypalPayload = PaypalPayload
  { _pplTotalPages :: Int
  , _pplPage :: Int
  , _pplDetails :: [PaypalRecord]
  }
  deriving stock (Show)
makeClassy ''PaypalPayload
instance FromJSON PaypalPayload where
  parseJSON = withObject "PaypalPayload" deser
    where
      deser obj =
        PaypalPayload
          <$> obj
          .: "total_pages"
          <*> obj
          .: "page"
          <*> (obj .: "transaction_details" >>= parseJSONList)
