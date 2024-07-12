module AppTypes where

import Account.Downloadable
import Hledger (Journal)
import JournalParser (getLatestDay)
import MyPrelude
import Options.Applicative (ReadM, maybeReader)
import RIO.Time
import SupportedAccount
import Types

newtype AppException = AppException String
instance Show AppException where
  show (AppException msg) = "fatal error: " <> msg
instance Exception AppException

data Task = DownloadRecordsTask | ImportRecordTask deriving stock (Show, Eq)
data PostImport = ArchiveRawRecords | ImportToJournal | DryImportToJournal
  deriving stock (Show, Eq)

data AppEnv = AppEnv
  { _appTasks :: NonEmpty Task
  , _appPostImports :: [PostImport]
  , _appAccounts :: NonEmpty (SupportedAccountAlias, SupportedAccountInfo)
  , _appStart :: Maybe Day
  , _appEnd :: Day
  , _appImportRule :: ImportRule
  , _appJournal :: Journal
  }
  deriving stock (Show)
makeClassy ''AppEnv

instance
  (HasAppEnv env) =>
  (HasAppEnv (WithLogFunc env))
  where
  appEnv = wEnv . appEnv

instance HasImportRule AppEnv where
  importRule = appImportRule

hasTask :: (HasAppEnv env) => Task -> RIO env Bool
hasTask t = view appTasks <&> elem t

hasPostImport :: (HasAppEnv env) => PostImport -> RIO env Bool
hasPostImport p = view appPostImports <&> elem p

runTask ::
  (HasAppEnv env, HasLogFunc env) =>
  Task ->
  (SupportedAccountAlias -> RIO env ()) ->
  RIO env ()
runTask task run = whenM (hasTask task) $ view appAccounts >>= traverse_ runWithLog
  where
    runWithLog (alias, _) =
      logInfo ("runing " <> displayShow task <> " on " <> displayShow alias)
        >> run alias

getSai ::
  (HasAppEnv env) =>
  SupportedAccountAlias ->
  RIO env SupportedAccountInfo
getSai alias = do
  accs <- view appAccounts
  (_, sai) <-
    forceJust
      (findOf folded ((alias ==) . fst) accs)
      (AppException $ "account" <> show alias <> " not found")
  return sai

getRecordDownloadInfo ::
  SupportedAccountAlias ->
  RIO (WithLogFunc AppEnv) RecordDownloadInfo
getRecordDownloadInfo alias = do
  sai <- getSai alias
  start <-
    fromMaybeM
      do
        j <- view appJournal
        getLatestDay j $ saiAccountName sai
      do view appStart
  end <- view appEnd
  let path = saiRawRecordsPath sai
  logInfo $ "downloading " <> displayShow alias <> " records from " <> displayShow start <> " to " <> displayShow end <> " into " <> displayShow path
  return $ RecordDownloadInfo path start end
