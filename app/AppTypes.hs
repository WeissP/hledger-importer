module AppTypes where

import Account.Downloadable
import Hledger (Journal)
import Hledger.Cli.Script (maximumMay)
import JournalParser (getLatestDay)
import MyPrelude
import RIO.FilePath (splitFileName, (<.>), (</>))
import RIO.FilePath qualified as F
import RIO.Text qualified as T
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

{- | Copied from Hledger library as it is not exported
Try to parse a couple of date string formats:
`YYYY-MM-DD`, `YYYY/MM/DD` or `YYYY.MM.DD`, with leading zeros required.
For internal use, not quite the same as the journal's "simple dates".
>>> parsedateM "2008/02/03"
Just 2008-02-03
>>> parsedateM "2008/02/03/"
Nothing
>>> parsedateM "2008/02/30"
Nothing
-}
parsedateM :: String -> Maybe Day
parsedateM s =
  asum
    [ parseTimeM True defaultTimeLocale "%Y-%m-%d" s
    , parseTimeM True defaultTimeLocale "%Y/%m/%d" s
    , parseTimeM True defaultTimeLocale "%Y.%m.%d" s
    ]

getRecordDownloadInfo ::
  SupportedAccountAlias ->
  RIO (WithLogFunc AppEnv) RecordDownloadInfo
getRecordDownloadInfo alias = do
  sai <- getSai alias
  start <-
    fromMaybeM
      do
        fromMaybeM
          do
            logInfo "Failed to parse .latest.*, trying to get latest day in journal files"
            j <- view appJournal
            getLatestDay j $ saiAccountName sai
          do
            logInfo "START is not specified, trying to get latest day from .latest.*"
            let (dir, n) = splitFileName $ saiImportRecordsPath sai
            let p = dir </> ".latest" <.> n
            t <- readFileUtf8 p
            logDebug $ "content of " <> display p <> ": " <> displayShow t
            return $ maximumMay $ mapMaybe (parsedateM . T.unpack . T.strip) (T.lines t)
      do view appStart
  end <- view appEnd
  let path = saiRawRecordsPath sai
  logInfo $ "downloading " <> displayShow alias <> " records from " <> displayShow start <> " to " <> displayShow end <> " into " <> displayShow path
  return $ RecordDownloadInfo path start end
