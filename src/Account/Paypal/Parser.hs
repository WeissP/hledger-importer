{-# OPTIONS_GHC -Wno-orphans #-}

module Account.Paypal.Parser where

import Account.Paypal.Types
import Conduit ((.|))
import Data.Aeson (eitherDecodeStrict, (.:))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Aeson.Lens (key, _Array, _String)
import Data.Conduit.Combinators qualified as C
import Data.Monoid (Endo)
import Data.Vector.Lens (vector)
import MyPrelude
import RIO.ByteString qualified as B
import RIO.ByteString qualified as BL
import RIO.Text qualified as T
import SupportedAccount
import Types

gets :: [J.Key] -> Parse PaypalRecord ByteString
gets keys = coerced . getStrIn keys

withItems' :: Parse J.Value J.Value -> Parse PaypalRecord ByteString
withItems' p = coerced . withItems p

instance SupportedAccount PaypalRecord where
  saAccAlias _ = paypalAccAlias
  saParse = C.linesUnboundedAscii .| C.mapM (forceRight . mapLeft (ImportException paypalAccAlias) . eitherDecodeStrict)
  saParseDate =
    gets ["transaction_info", "transaction_initiation_date"]
      <> gets ["transaction_info", "transaction_updated_date"]
  saParseCurrency =
    gets ["transaction_info", "transaction_amount", "currency_code"] . currencyL
  saParseAmount = gets ["transaction_info", "transaction_amount", "value"]
  saParseByLabel "event_code" = gets ["transaction_info", "transaction_event_code"]
  saParseByLabel "subject" = gets ["transaction_info", "transaction_subject"]
  saParseByLabel "email_address" = gets ["payer_info", "email_address"]
  saParseByLabel "alternate_full_name" =
    gets ["payer_info", "payer_name", "alternate_full_name"]
  saParseByLabel "item_name" = withItems' $ key "item_name"
  saParseByLabel "item_desc" = withItems' $ key "item_description"
  saParseByLabel lbl = gets $ T.split (',' ==) lbl <&> J.fromText
