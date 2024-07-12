module Account.Downloadable where

import Conduit
import Data.Aeson (ToJSON (..))
import Data.Aeson qualified as J
import Data.Conduit.Combinators qualified as C
import MyPrelude
import RIO.ByteString qualified as BL
import RIO.Time
import Types

data RecordDownloadInfo = RecordDownloadInfo
  { _rdiPath :: FilePath
  , _rdiStart :: Day
  , _rdiEnd :: Day
  }
  deriving stock (Show)
makeClassy ''RecordDownloadInfo
instance
  (HasRecordDownloadInfo env) =>
  (HasRecordDownloadInfo (WithLogFunc env))
  where
  recordDownloadInfo = wEnv . recordDownloadInfo

type ApiRecordEnv env = (HasLogFunc env, HasRecordDownloadInfo env)

class (ToJSON a) => ApiRecord env a | a -> env where
  arGet :: ConduitT () a (ResourceT (RIO env)) ()
  arInitEnv :: forall arEnv. (ApiRecordEnv arEnv) => Proxy a -> RIO arEnv env

downloadJSON :: forall a arEnv env. (ApiRecordEnv arEnv, ApiRecord env a) => Proxy a -> RIO arEnv ()
downloadJSON ty = do
  env <- arInitEnv ty
  p <- view rdiPath
  runRIO env
    $ runConduitRes
    $ arGet
    .| C.map (BL.toStrict . J.encode @a)
    .| C.intersperse "\n"
    .| sinkFile p

data DownloadException = DownloadException SupportedAccountAlias String
instance Show DownloadException where
  show (DownloadException alias msg) =
    "fatal error during Downloading account " <> show alias <> ": " <> msg
instance Exception DownloadException
