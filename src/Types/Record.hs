module Types.Record where

import Data.ByteString.Builder (byteString)
import Hledger (AccountName, Status (..))
import MyPrelude

separator :: Builder
separator = ";"

data Currency
  = UnknownCurrency ByteString
  | Euro
  | JPY
  | RMB
  | USD
  | TWD
  deriving stock (Generic, Show)

instance Display Currency where
  display (UnknownCurrency t) = wrapDisplay "UnknownCurrency[" t "]"
  display Euro = "€"
  display JPY = "JPY"
  display RMB = "RMB"
  display USD = "$"
  display TWD = "TWD"

currencyL :: Iso' ByteString Currency
currencyL = iso parse buildUTF8
  where
    parse "EUR" = Euro
    parse "€" = Euro
    parse "USD" = USD
    parse "$" = USD
    parse "JPY" = JPY
    parse raw = UnknownCurrency raw

data ImportComment = ImportComment
  {_icStr :: Maybe ByteString, _icTags :: [(ByteString, ByteString)]}
  deriving (Generic, Default, Show)
makeLenses ''ImportComment
instance Display ImportComment where
  display (ImportComment {..}) =
    display
      -- add quotes to escape separator
      $ show
      $ show
      $ display (maybe "" (<> ", ") _icStr)
      <> displayWithSep ", " ((\(tag, txt) -> tag <> ": " <> txt) <$> _icTags)

data ImportPosting = ImportPosting
  { _ipAcc :: AccountName
  , _ipAmount :: ByteString
  , _ipComment :: ImportComment
  , _ipCurrency :: Currency
  }
makeLenses ''ImportPosting
instance Display ImportPosting where
  display (ImportPosting {..}) = displayWithSep separator $ display _ipAcc :| [display _ipAmount, display _ipComment, display _ipCurrency]

data ImportRecord = ImportRecord
  { _irDate :: ByteString
  , _irStatus :: Status
  , _irAmount :: ByteString
  , _irDesc :: ByteString
  , _irPosting1 :: ImportPosting
  , _irPosting2 :: ImportPosting
  }
makeLenses ''ImportRecord
instance Display ImportRecord where
  display (ImportRecord {..}) = displayWithSep separator $ display _irDate :| [displayShow _irStatus, display _irAmount, display _irDesc, display _irPosting1, display _irPosting2]
