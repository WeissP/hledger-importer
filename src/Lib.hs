module Lib where

import Account.Comdirect
import Account.Commerzbank
import Account.Downloadable (HasRecordDownloadInfo, downloadJSON)
import Account.Paypal
import Conduit
import Data.Conduit.Combinators qualified as C
import MyPrelude
import SupportedAccount
import Types

importRecords ::
  forall env a m.
  ( LoggerM env m
  , HasImportRule env
  , SupportedAccount a
  , MonadThrow m
  ) =>
  Proxy a ->
  m ()
importRecords ty = do
  let alias = saAccAlias ty
  sai@(SupportedAccountInfo {..}) <- supportedAccountInfo alias
  writeRule sai
  runConduitRes
    $ sourceFile saiRawRecordsPath
    .| C.map dropBOM
    .| saParse
    -- .| C.mapM (\x -> logDebug ("reading record: " <> display x) >> return x)
    .| C.concatMapM (saToImportRecord @a)
    .| C.map buildUTF8
    .| C.intersperse "\n"
    .| sinkFile saiImportRecordsPath

writeRule :: (MonadIO m) => SupportedAccountInfo -> m ()
writeRule (SupportedAccountInfo {..}) = writeFileUtf8Builder saiRulePath saiRule

dispatchAcc ::
  (forall a. (SupportedAccount a) => Proxy a -> m ()) -> SupportedAccountAlias -> m ()
dispatchAcc f Paypal = f (Proxy @PaypalRecord)
dispatchAcc f CommerzBank = f (Proxy @CommerzbankRecord)
dispatchAcc f Comdirect = f (Proxy @ComdirectRecord)

downloadRecord :: (HasLogFunc env, HasRecordDownloadInfo env) => SupportedAccountAlias -> RIO env ()
downloadRecord Paypal = downloadJSON $ Proxy @PaypalRecord
downloadRecord Comdirect = downloadJSON $ Proxy @ComdirectRecord
downloadRecord CommerzBank = return ()
