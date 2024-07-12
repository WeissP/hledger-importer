module Opts (getAppEnv) where

import AppTypes
import Hledger (getCurrentDay)
import Hledger.Cli (Journal)
import MyPrelude
import Options.Applicative
import RIO.NonEmpty.Partial qualified as NE'
import RIO.Time (Day)
import RuleParser.Org.Conv (readOrgConfig, toImportRule)
import SupportedAccount (supportedAccountInfo)
import Types

data Cli = Cli
  { cliTasks :: NonEmpty Task
  , cliPostImports :: [PostImport]
  , cliAccounts :: NonEmpty SupportedAccountAlias
  , cliStart :: Maybe Day
  , cliEnd :: Maybe Day
  , cliConfigPath :: Maybe FilePath
  }
  deriving stock (Show)

parseStartDay :: Parser Day
parseStartDay =
  option
    auto
    ( long "start"
        <> short 's'
        <> metavar "START"
        <> help "only consider transactions after the given day"
    )

parseEndDay :: Parser Day
parseEndDay =
  option
    auto
    ( long "end"
        <> short 'e'
        <> metavar "END"
        <> help "only consider transactions before the given day"
    )

parseSAAlias :: Parser SupportedAccountAlias
parseSAAlias =
  option
    auto
    ( long "account"
        <> short 'a'
        <> metavar "ACCOUNT"
        <> help ("account alias, only following accounts are supported: " <> show [minBound .. maxBound :: SupportedAccountAlias])
    )

parseConfigPath :: Parser FilePath
parseConfigPath =
  option
    auto
    ( long "config"
        <> short 'c'
        <> metavar "CONFIG"
        <> help "the path to the config, if not specified, env value LEDGER_HELPER_RULE_PATH will be read"
    )

parseTask :: Parser Task
parseTask =
  flag' DownloadRecordsTask (long "download" <> help "download records")
    <|> flag' ImportRecordTask (long "import" <> help "import records to hledger journal file")

parsePostImport :: Parser PostImport
parsePostImport =
  flag' ArchiveRawRecords (long "archive-raw-records" <> help "archive raw records after successfully importing")
    <|> flag' ImportToJournal (long "import-to-journal" <> help "import records to hledger journal file after successfully importing")
    <|> flag' DryImportToJournal (long "dry-import-to-journal" <> help "output records to terminal after successfully importing")

parseCli :: Parser Cli
parseCli =
  Cli
    <$> nonEmptyA parseTask
    <*> many parsePostImport
    <*> nonEmptyA parseSAAlias
    <*> optional parseStartDay
    <*> optional parseEndDay
    <*> optional parseConfigPath

nonEmptyA :: (Alternative f) => f a -> f (NonEmpty a)
nonEmptyA g = some g <&> NE'.fromList

cliOptions :: ParserInfo Cli
cliOptions =
  info
    (parseCli <**> helper)
    ( fullDesc
        <> progDesc "A hledger helper to manage accounts"
        <> header "hledger-helper - A hledger helper to manage accounts"
    )

getAppEnv :: (HasLogFunc env) => Journal -> RIO env AppEnv
getAppEnv j = do
  lf <- view logFuncL
  Cli {..} <- liftIO $ execParser cliOptions
  cfgPath <- fromMaybeM (getEnv "LEDGER_HELPER_RULE_PATH") (return cliConfigPath)
  logInfo $ "import rule path: " <> display cfgPath
  ir <- readOrgConfig cfgPath >>= toImportRule
  accounts <- runRIO (WithLogFunc lf ir) $ forM cliAccounts $ \a -> do
    let name = a ^. re _SupportedAccountAlias
    sai <- supportedAccountInfo name
    return (a, sai)
  end <- fromMaybeM (liftIO getCurrentDay) (return cliEnd)
  return $ AppEnv cliTasks cliPostImports accounts cliStart end ir j
