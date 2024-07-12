module Account.Commerzbank where

import Conduit
import Data.ByteString.Internal (c2w)
import Data.Conduit.Combinators qualified as C
import Data.Csv qualified as Csv
import Data.Csv.Conduit (fromNamedCsvStreamErrorNoThrow)
import MyPrelude
import SupportedAccount
import Types

newtype CommerzbankRecord = CommerzbankRecord
  {fromCommerzbankRecord :: Map Text ByteString}
  deriving stock (Show)
  deriving newtype (Csv.FromNamedRecord)
instance Display CommerzbankRecord where display = displayShow

parseByName :: Text -> Parse CommerzbankRecord ByteString
parseByName t = (coerced :: Iso' CommerzbankRecord (Map Text ByteString)) . ix t

commerzbankAccAlias :: AccAlias
commerzbankAccAlias = "CommerzBank"

instance SupportedAccount CommerzbankRecord where
  saAccAlias _ = commerzbankAccAlias
  saParse = fromNamedCsvStreamErrorNoThrow (Csv.DecodeOptions (c2w ';')) .| C.mapM f
    where
      f (Left err) = throwM $ ImportException commerzbankAccAlias (show err)
      f (Right r) = return r
  saParseDate = parseByName "Buchungstag" <> parseByName "Wertstellung"
  saParseCurrency = parseByName "Währung" . currencyL
  saParseAmount = parseByName "Betrag"
  saParseByLabel = parseByName
