{-# OPTIONS_GHC -Wno-orphans #-}

module Account.Comdirect.Downloader () where

import Account.Comdirect.Types
import Account.Downloadable
import Conduit
import Data.Aeson qualified as J
import Data.Aeson.Encoding qualified as J
import Data.Aeson.Lens (key, nth, _Array, _String)
import Data.Conduit.Combinators qualified as C
import Data.List (intercalate)
import Data.Pass
import MyPrelude
import Network.Wreq (FormParam ((:=)), auth, getWith, header, oauth2Bearer, patchWith, postWith, responseBody, responseHeader)
import Network.Wreq qualified as NW
import RIO.ByteString (getLine, putStr)
import RIO.ByteString.Lazy qualified as BL
import RIO.Process (mkDefaultProcessContext)
import RIO.Time (Day (ModifiedJulianDay))

instance ApiRecord (WithLogFunc ComdirectAccessEnv) ComdirectRecord where
  arGet = getComdirectRecords
  arInitEnv _ = do
    lf <- view logFuncL
    ctx <- mkDefaultProcessContext
    let pEnv = PassEnv lf ctx
    pr <- runRIO pEnv (getPassRecord "comdirect")
    cu <- runRIO pr getComdirectUser
    rdi <- view recordDownloadInfo
    let env = ComdirectEnv cu def
    runRIO (WithLogFunc lf env) do
      opt <- accessHeaders
      return $ WithLogFunc lf $ ComdirectAccessEnv (ComdirectAccessOpt opt) def rdi

getAccoundID :: (HasLogFunc env, HasComdirectAccessOpt env, HasComdirectApi env, MonadIO m, MonadReader env m) => m String
getAccoundID = do
  logInfo "getting comdirect account ID"
  url <- withAPI ["banking", "clients", "user", "v1", "accounts", "balances"]
  opts <- view $ comdirectAccessOpt . coerced
  r <- liftIO $ getWith opts url
  return
    $ r
    ^. (responseBody . key "values" . _Array . ix 0)
    . (key "accountId" . _String . re isoStrT)

getComdirectRecords ::
  forall env m.
  ( HasLogFunc env
  , HasRecordDownloadInfo env
  , HasComdirectAccessOpt env
  , HasComdirectApi env
  , MonadUnliftIO m
  , MonadThrow m
  , MonadReader env m
  ) =>
  ConduitT () ComdirectRecord m ()
getComdirectRecords = do
  start <- view rdiStart
  end <- view rdiEnd
  accId <- getAccoundID
  url <- withAPI ["banking", "v1", "accounts", accId, "transactions"]
  opts :: NW.Options <- view $ comdirectAccessOpt . coerced
  let
    yieldRecords s = yield s .| C.map (view cpDetails) .| C.concat
    get :: Int -> Int -> IO ComdirectPayload
    get offset limit =
      getWith
        ( opts
            & NW.params
            .~ [ ("transactionState", "BOOKED")
               , ("min-bookingDate", tshow start)
               , ("max-bookingDate", tshow end)
               , ("paging-first", tshow offset)
               , ("paging-count", tshow limit)
               ]
        )
        url
        >>= NW.asJSON
        <&> view responseBody
  fstBatch <- liftIO $ get 0 500
  yieldRecords fstBatch
  when (fstBatch ^. cpLimit > 500)
    $ liftIO (get 500 (fstBatch ^. cpLimit - 500))
    >>= yieldRecords

accessHeaders ::
  ( HasLogFunc env
  , HasComdirectUser env
  , HasComdirectApi env
  , MonadReader env m
  , MonadIO m
  ) =>
  m NW.Options
accessHeaders = do
  let authHeader =
        NW.defaults
          & header "Accept"
          .~ ["application/json"]
          & header "Accept-Language"
          .~ ["en_US"]
          & header "Content-Type"
          .~ ["application/x-www-form-urlencoded"]
  oauthTokenUrl <- view caOauthTokenUrl
  ComdirectUser {..} <- view comdirectUser
  let authPost = liftIO . postWith authHeader oauthTokenUrl
  logDebug "getting comdirect token"
  tkR <-
    authPost
      [ "client_id" := _cdiClientID
      , "client_secret" := _cdiClientSecret
      , "username" := _cdiUsername
      , "password" := _cdiPassword
      , "grant_type" := ("password" :: String)
      ]
  let token = tkR ^. responseBody . key "access_token" . _String
      sessionSegs = ["session", "clients", _cdiClientID ^. re isoStrBs, "v1", "sessions"]
  tokenHeaders <- authenticated token
  logDebug "getting comdirect session ID"
  sidR <- withAPI sessionSegs >>= liftIO . getWith tokenHeaders
  let sid = sidR ^. responseBody . nth 0 . key "identifier" . _String
      sidObj =
        J.pairs
          ( ("identifier" J..= sid)
              <> ("sessionTanActive" J..= True)
              <> ("activated2FA" J..= True)
          )
  validateUrl <- withAPI $ sessionSegs <> [sid ^. re isoStrT, "validate"]
  logDebug "getting comdirect challenge ID"
  cIDR <-
    liftIO
      $ postWith
        ( tokenHeaders
            & (header "x-once-authentication-info" .~ ["{\"typ\": \"P_TAN_PUSH\"}"])
        )
        validateUrl
        sidObj
  let cID = cIDR ^. responseHeader "x-once-authentication-info" . key "id" . _String
      cIDHeader = BL.toStrict $ J.encodingToLazyByteString $ J.pairs ("id" J..= cID)
  logDebug "waiting comdirect authentication"
  putStr "Please allow comdirect access on APP\n"
  _ <- getLine
  logDebug "activating comdirect Session"
  sidUrl <- withAPI $ sessionSegs <> [sid ^. re isoStrT]
  liftIO
    $ patchWith
      (tokenHeaders & header "x-once-authentication-info" .~ [cIDHeader])
      sidUrl
      sidObj
  logDebug "getting comdirect access headers"
  accessTkR <-
    authPost
      [ "client_id" := _cdiClientID
      , "client_secret" := _cdiClientSecret
      , "token" := token
      , "grant_type" := ("cd_secondary" :: String)
      ]
  authenticated $ accessTkR ^. responseBody . key "access_token" . _String

withAPI :: (HasComdirectApi env, MonadReader env m) => [String] -> m String
withAPI segs = do
  url <- view caApiUrl
  return $ intercalate "/" (url : segs)

authenticated :: (HasComdirectApi env, MonadReader env m) => Text -> m NW.Options
authenticated token = do
  rid <- view caRequestID
  return
    $ NW.defaults
    & header "Accept"
    .~ ["application/json"]
    & header "Content-Type"
    .~ ["application/json"]
    & header "x-http-request-info"
    .~ [rid]
    & auth
    ?~ oauth2Bearer (encodeUtf8 token)

getComdirectUser :: (HasPassRecord env, MonadReader env m, MonadThrow m) => m ComdirectUser
getComdirectUser =
  ComdirectUser
    <$> prop "client_id"
    <*> prop "client_secret"
    <*> prop "Zugangsnummer"
    <*> prop "PIN"
  where
    prop = fmap (review isoBsT) . prProp_

comdirectTest :: IO ()
comdirectTest = do
  logOptions <- logOptionsHandle stderr True
  withLogFunc logOptions $ \lf -> do
    ctx <- mkDefaultProcessContext
    let pEnv = PassEnv lf ctx
    pr <- runRIO pEnv (getPassRecord "comdirect")
    cu <- runRIO pr getComdirectUser
    let env = ComdirectEnv cu def
    runRIO (WithLogFunc lf env) do
      hd <- accessHeaders
      logInfo $ displayShow hd
