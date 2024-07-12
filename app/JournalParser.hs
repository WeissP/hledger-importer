module JournalParser where

import Hledger.Cli.Script
import MyPrelude
import RIO.List qualified as L
import Types

getLatestDay :: (HasLogFunc env) => Journal -> AccountName -> RIO env Day
getLatestDay j n = logInfo "START is not specified, trying to get latest day in journal files" >> fromMaybeM dftDay (return d)
  where
    q = Acct $ toRegex' n
    ts = filter (q `matchesTransaction`) $ jtxns j
    d = L.maximumMaybe (ts <&> tdate)
    dftDay = do
      now <- liftIO getCurrentDay
      let dft = addDays (-(365 * 2)) now
      logWarn $ "No transaction found for account " <> displayShow n <> ", use " <> displayShow dft <> " as starting day"
      return dft

getFromJournal :: (HasLogFunc env) => env -> (Journal -> RIO env a) -> IO a
getFromJournal e g = withJournalDo def $ \j -> runRIO e (g j)
