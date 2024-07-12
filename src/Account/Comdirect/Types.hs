module Account.Comdirect.Types where

import Account.Downloadable
import Data.Aeson (FromJSON, ToJSON, parseJSONList, withObject, (.:))
import Data.Aeson qualified as J
import MyPrelude
import Network.Wreq qualified as NW
import Types

comdirectAccAlias :: AccAlias
comdirectAccAlias = "Comdirect"

newtype ComdirectRecord = ComdirectRecord J.Value
  deriving stock (Show)
  deriving newtype (FromJSON, ToJSON)
instance Display ComdirectRecord where
  textDisplay (ComdirectRecord v) = tshow v

data ComdirectPayload = ComdirectPayload
  { _cpLimit :: Int
  , _cpOffset :: Int
  , _cpDetails :: [ComdirectRecord]
  }
  deriving stock (Show)
makeClassy ''ComdirectPayload
instance FromJSON ComdirectPayload where
  parseJSON = withObject "ComdirectPayload" deser
    where
      deser obj =
        ComdirectPayload
          <$> (obj .: "paging" >>= (.: "matches"))
          <*> (obj .: "paging" >>= (.: "index"))
          <*> (obj .: "values" >>= parseJSONList)

debugCp :: ComdirectPayload -> Utf8Builder
debugCp cp = displayShow (cp ^. cpOffset, cp ^. cpLimit, listDisplay dates)
  where
    recordsJSON :: [J.Value] = cp ^. cpDetails . coerced
    dates = recordsJSON ^.. traverse . getStrIn ["bookingDate"]

newtype ComdirectAccessOpt = ComdirectAccessOpt NW.Options deriving (Show)
makeClassy ''ComdirectAccessOpt
instance
  (HasComdirectAccessOpt env) =>
  (HasComdirectAccessOpt (WithLogFunc env))
  where
  comdirectAccessOpt = wEnv . comdirectAccessOpt

data ComdirectApi = ComdirectApi
  { _caOauthTokenUrl :: String
  , _caApiUrl :: String
  , _caRequestID :: ByteString
  }
  deriving stock (Show)
makeClassy ''ComdirectApi
instance Default ComdirectApi where
  def =
    ComdirectApi
      "https://api.comdirect.de/oauth/token"
      "https://api.comdirect.de/api"
      "{\"clientRequestId\":{\"sessionId\":\"8f7db49e-de15-4fa8-b529-a35af24355d0\",\"requestId\":\"8f7db49e\"}}"
instance
  (HasComdirectApi env) =>
  (HasComdirectApi (WithLogFunc env))
  where
  comdirectApi = wEnv . comdirectApi

data ComdirectUser = ComdirectUser
  { _cdiClientID :: ByteString
  , _cdiClientSecret :: ByteString
  , _cdiUsername :: ByteString
  , _cdiPassword :: ByteString
  }
  deriving stock (Show)
makeClassy ''ComdirectUser
instance
  (HasComdirectUser env) =>
  (HasComdirectUser (WithLogFunc env))
  where
  comdirectUser = wEnv . comdirectUser

data ComdirectDownloadState = ComdirectDownloadState {}

data ComdirectEnv = ComdirectEnv
  { _ceUser :: ComdirectUser
  , _ceApi :: ComdirectApi
  }
  deriving stock (Generic, Show)
makeClassy ''ComdirectEnv
instance HasComdirectApi ComdirectEnv where
  comdirectApi = ceApi
instance HasComdirectUser ComdirectEnv where
  comdirectUser = ceUser

data ComdirectAccessEnv = ComdirectAccessEnv
  { _caeOpt :: ComdirectAccessOpt
  , _caeApi :: ComdirectApi
  , _caeRecordDownloadInfo :: RecordDownloadInfo
  }
  deriving stock (Generic, Show)
makeClassy ''ComdirectAccessEnv
instance HasComdirectApi ComdirectAccessEnv where
  comdirectApi = caeApi
instance HasComdirectAccessOpt ComdirectAccessEnv where
  comdirectAccessOpt = caeOpt
instance HasRecordDownloadInfo ComdirectAccessEnv where
  recordDownloadInfo = caeRecordDownloadInfo
