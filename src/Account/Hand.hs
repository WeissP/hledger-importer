module Account.Hand where

import Conduit
import Data.ByteString.Internal (c2w)
import Data.Conduit.Combinators qualified as C
import Data.Csv qualified as Csv
import Data.Csv.Conduit (fromNamedCsvStreamErrorNoThrow)
import MyPrelude
import SupportedAccount
import Types

newtype HandRecord = HandRecord
  {fromHandRecord :: Map Text ByteString}
  deriving stock (Show)
  deriving newtype (Csv.FromNamedRecord)
instance Display HandRecord where display = displayShow

parseByName :: Text -> Parse HandRecord ByteString
parseByName t = (coerced :: Iso' HandRecord (Map Text ByteString)) . ix t

handAccAlias :: AccAlias
handAccAlias = "Hand"

instance SupportedAccount HandRecord where
  saAccAlias _ = handAccAlias
  saParse = fromNamedCsvStreamErrorNoThrow (Csv.DecodeOptions (c2w ';')) .| C.mapM f
    where
      f (Left err) = throwM $ ImportException handAccAlias (show err)
      f (Right r) = return r
  saParseDate = parseByName "date"
  saParseCurrency = to $ const Euro
  saParseAmount = to (const "") <> parseByName "amount" -- input positive amount is more intuitive
  saParseByLabel = parseByName
