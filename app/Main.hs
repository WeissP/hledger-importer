import AppTypes

import Hledger.Cli qualified as H
import Hledger.Cli.Script (withJournalDo)
import Lib
import MyPrelude
import Opts
import RIO.Directory
import RIO.FilePath
import RIO.Time
import Types

main :: IO ()
main = do
  logLevelStr <- getEnv "LOG_LEVEL"
  logLevel <-
    forceJust (readMaybe logLevelStr)
      $ AppException ("Could not parse log level: " <> logLevelStr)
  logOptions <- logOptionsHandle stderr True <&> setLogMinLevel logLevel
  withLogFunc logOptions $ \lf -> withJournalDo def $ \j -> do
    env <- runRIO (WithLogFunc lf ()) (getAppEnv j)
    runRIO (WithLogFunc lf env) do
      runTask DownloadRecordsTask $ \a -> do
        rdi <- getRecordDownloadInfo a
        runRIO (WithLogFunc lf rdi) $ downloadRecord a
      runTask ImportRecordTask $ \a -> dispatchAcc importRecords a >> postImport a

postImport ::
  (HasAppEnv env, HasLogFunc env) =>
  SupportedAccountAlias ->
  RIO env ()
postImport alias = do
  logInfo $ "Account " <> displayShow alias <> " successfully imported"
  SupportedAccountInfo {..} <- getSai alias
  whenM (hasPostImport DryImportToJournal)
    $ executeHLedger ["import", saiImportRecordsPath, "--dry-run"]
  whenM (hasPostImport ImportToJournal)
    $ executeHLedger ["import", saiImportRecordsPath]
  whenM (hasPostImport ArchiveRawRecords) do
    t <- getCurrentTime
    let s = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" t
    createDirectoryIfMissing True saiArchivedRawRecordsDir
    renameFile
      saiRawRecordsPath
      (saiArchivedRawRecordsDir </> addExtension s (takeExtension saiRawRecordsPath))

executeHLedger :: (HasAppEnv env, HasLogFunc env) => [String] -> RIO env ()
executeHLedger args = do
  j <- view appJournal
  logDebug $ "executing HLedger command with args: " <> displayShow args
  opts <- liftIO $ H.argsToCliOpts args []
  liftIO $ H.importcmd opts j
