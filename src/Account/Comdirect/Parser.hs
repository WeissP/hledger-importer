{-# OPTIONS_GHC -Wno-orphans #-}

module Account.Comdirect.Parser where

import Account.Comdirect.Types
import Conduit ((.|))
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as J
import Data.Conduit.Combinators qualified as C
import MyPrelude
import RIO.Text qualified as T
import SupportedAccount
import Types

gets :: [J.Key] -> Parse ComdirectRecord ByteString
gets keys = coerced . getStrIn keys

instance SupportedAccount ComdirectRecord where
  saAccAlias _ = comdirectAccAlias
  saParse = C.linesUnboundedAscii .| C.mapM (forceRight . mapLeft (ImportException comdirectAccAlias) . J.eitherDecodeStrict)
  saParseDate = gets ["bookingDate"] <> gets ["valutaDate"]
  saParseCurrency = gets ["amount", "unit"] . currencyL
  saParseAmount = gets ["amount", "value"]
  saParseByLabel "remittanceInfo" = gets ["remittanceInfo"]
  saParseByLabel "holderName" = gets ["remitter", "holderName"]
  saParseByLabel "creditorHolderName" = gets ["creditor", "holderName"]
  saParseByLabel "transactionType" = gets ["transactionType", "key"]
  saParseByLabel lbl = gets $ T.split (',' ==) lbl <&> J.fromText
