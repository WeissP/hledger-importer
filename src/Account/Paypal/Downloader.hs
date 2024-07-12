{-# OPTIONS_GHC -Wno-orphans #-}

module Account.Paypal.Downloader where

import Account.Downloadable
import Account.Paypal.Types
import Conduit
import Data.Aeson.Lens qualified as J
import Data.Conduit.Combinators qualified as C
import Data.Pass
import Data.Time.Format.ISO8601 (iso8601Show)
import MyPrelude
import Network.Wreq (auth, getWith, header, oauth2Bearer, param, responseBody)
import Network.Wreq qualified as W
import RIO.Process (mkDefaultProcessContext)
import RIO.State
import RIO.Text qualified as T
import RIO.Time

instance ApiRecord (WithLogFunc PaypalEnv) PaypalRecord where
  arGet = getPaypalRecords
  arInitEnv _ = do
    lf <- view logFuncL
    ctx <- mkDefaultProcessContext
    let pEnv = PassEnv lf ctx
    pr <- runRIO pEnv (getPassRecord "paypal")
    tk <- runRIO pr getPaypaltoken
    rdi <- view recordDownloadInfo
    st <- newSomeRef def
    return $ WithLogFunc lf (PaypalEnv pr tk rdi st)

getPaypaltoken :: (HasPassRecord env) => RIO env PaypalToken
getPaypaltoken = do
  clientId <- prProp_ "client-id"
  secret <- prProp_ "secret"
  let
    opts =
      W.defaults
        & header "Accept"
        .~ ["application/json"]
        & header "Accept-Language"
        .~ ["en_US"]
        & auth
        ?~ W.basicAuth (encodeUtf8 clientId) (encodeUtf8 secret)
  r <- liftIO $ W.postWith opts "https://api-m.paypal.com/v1/oauth2/token" ("grant_type=client_credentials" :: ByteString)
  return $ r ^. W.responseBody . J.key "access_token" . J._String . re isoBsT . coerced

getPaypalRecords ::
  ( HasPaypalToken env
  , HasRecordDownloadInfo env
  , MonadState PaypalDownloadState m
  , MonadUnliftIO m
  , MonadReader env m
  ) =>
  ConduitT () PaypalRecord m ()
getPaypalRecords = do
  goalEnd <- view rdiEnd
  goalStart <- view rdiStart
  pdsStart .= goalStart
  pdsEnd .= addDays 30 goalStart `min` goalEnd
  pdsPage .= 1
  let batch = flip C.repeatWhileM isJust do
        start <- use pdsStart
        if start > goalEnd
          then return Nothing
          else do
            end <- use pdsEnd
            page <- use pdsPage
            res <- getPaypalRecordsRaw start end page
            if page < res ^. pplTotalPages
              then do
                pdsPage .= page + 1
              else do
                pdsPage .= 1
                pdsStart .= addDays 1 end
                pdsEnd .= addDays 30 end `min` goalEnd
            return $ Just $ res ^. pplDetails
  batch .| C.concat .| C.concat

getPaypalRecordsRaw ::
  (MonadReader env m, HasPaypalToken env, MonadIO m) =>
  Day ->
  Day ->
  Int ->
  m PaypalPayload
getPaypalRecordsRaw start end page = do
  tk <- view $ paypalToken . coerced
  let dateStr d = T.pack $ iso8601Show d <> "T00:00:00+0200"
      opts =
        W.defaults
          & header "Content-Type"
          .~ ["application/json"]
          & auth
          ?~ oauth2Bearer tk
          & param "start_date"
          .~ [dateStr start]
          & param "end_date"
          .~ [dateStr end]
          & param "fields"
          .~ ["all"]
          & param "page_size"
          .~ ["500"]
          & param "page"
          .~ [tshow page]
  r <-
    liftIO
      $ getWith opts "https://api-m.paypal.com/v1/reporting/transactions"
      >>= W.asJSON
  return $ r ^. responseBody
