module SupportedAccount (SupportedAccount (..), Parse, supportedAccountInfo) where

import Catch
import Conduit (ConduitT)
import Data.Monoid (Endo)
import Data.Monoid.Extra (First)
import Hledger (Status (..))
import MyPrelude
import RIO.FilePath ((</>))
import RIO.List qualified as L
import RIO.NonEmpty qualified as NE
import Text.Regex.TDFA ((=~))
import Types

class (Display a) => SupportedAccount a where
  saAccAlias :: Proxy a -> AccAlias
  saParse :: (MonadThrow m) => ConduitT ByteString a m ()
  saParseDate :: Parse a ByteString
  saParseCurrency :: Parse a Currency
  saParseAmount :: Parse a ByteString
  saParseByLabel :: TaLabel -> Parse a ByteString
  saToImportRecord ::
    forall env m.
    (ImporterM env m, LoggerM env m) =>
    a ->
    m [ImportRecord]
  saToImportRecord a = do
    let alias = saAccAlias (Proxy @a)
    ar <- accRule alias
    isFiltered <- catchItem (parseOne a . saParseByLabel) (ar ^. accFilter)
    case isFiltered of
      Just _ -> return []
      Nothing -> do
        logDebug $ "parsing record: " <> display a
        parsered <- runMaybeT do
          date <- logIfNothing (parseOne a saParseDate) (logError "could not parse date")
          (amt, amt1, amt2) <-
            logIfNothing (parseNonEmpty a saParseAmount) (logError "could not parse amount")
              <&> \case
                x :| [] -> (x, "", "")
                x :| y : _ -> ("", x, y)
          cur :| restCurs <- logIfNothing (parseNonEmpty a saParseCurrency) (logError "could not parse currency")
          knownCmtTags <- cmtTags a (ar ^. accCommentTags)
          let
            p1 = ImportPosting (ar ^. accName) amt1 (def & icTags .~ knownCmtTags) cur
            cur2 = fromMaybe cur (listToMaybe restCurs)

          toAccMay <- catchItem (parseOne a . saParseByLabel) (ar ^. accCatcher)
          case toAccMay of
            Just toAcc -> do
              desc <- catchDesc toAcc
              let p2 = ImportPosting toAcc amt2 def cur2
              return $ ImportRecord date Cleared amt desc p1 p2
            Nothing -> do
              unknownCmtTags <-
                cmtTags a
                  $ (ar ^. accUnknownCommentLabels)
                  <&> \lbl -> (encodeUtf8 lbl, lbl)
              let p2 = ImportPosting "unknown" amt2 (def & icTags .~ unknownCmtTags) cur2
              return $ ImportRecord date Pending amt "unknown" p1 p2

        case parsered of
          Just res | res ^. irAmount . to isZeroAmount -> return []
          Just res -> return [res]
          Nothing -> logError ("couldn't import record: " <> display a) $> []

isZeroAmount :: ByteString -> Bool
isZeroAmount s = s =~ ("^[0.,]+$" :: String)

accRule :: (ImporterM env m) => AccAlias -> m AccountRule
accRule alias = do
  rules <- view $ importRule . irAccRules
  let rule = L.find (\r -> r ^. accAlias == alias) rules
  forceJust rule $ ImportException alias "could not find corresponding rule"

supportedAccountInfo :: (ImporterM env m) => AccAlias -> m SupportedAccountInfo
supportedAccountInfo saiAccAlias = do
  ir <- view importRule
  ar <- accRule saiAccAlias
  let alias = saiAccAlias ^. isoBsT . re isoStrT
      saiAccountName = ar ^. accName
      rootDir = ir ^. irFinanceDir </> "records" </> alias
      saiRawRecordsPath = rootDir </> ar ^. accRawRecordsName
      saiArchivedRawRecordsDir = rootDir </> "raw_records"
      saiRulePath = rootDir </> (ar ^. accImportRecordsName <> ".rules")
      saiImportRecordsPath = rootDir </> ar ^. accImportRecordsName
      saiRule =
        wrapDisplay
          "fields  date,status,amount,description,account1,amount1,comment1,currency1,account2,amount2,comment2,currency2\nseparator ;\ndate-format  "
          (ar ^. accDateFormat)
          ""
  return $ SupportedAccountInfo {..}

parseOne :: (Monad m) => s -> Getting (First a) s a -> MaybeT m a
parseOne s p = hoistMaybe $ s ^? p

parseNonEmpty :: (Monad m) => s -> Getting (Endo [a]) s a -> MaybeT m (NonEmpty a)
parseNonEmpty s p = hoistMaybe $ NE.nonEmpty $ s ^.. p

cmtTags ::
  (ImporterM env m, SupportedAccount a) =>
  a ->
  [(ByteString, TaLabel)] ->
  m [(ByteString, ByteString)]
cmtTags a lbls =
  catMaybes
    <$> forM
      lbls
      \(tag, lbl) -> runMaybeT do
        str <- parseOne a (saParseByLabel lbl)
        return (tag, str)
