module Types.Account where

import Hledger (AccountName)
import MyPrelude
import Types.ImportRule

data SupportedAccountInfo = SupportedAccountInfo
  { saiAccAlias :: AccAlias
  , saiAccountName :: AccountName
  , saiRawRecordsPath :: FilePath
  , saiArchivedRawRecordsDir :: FilePath
  , saiRulePath :: FilePath
  , saiRule :: Utf8Builder
  , saiImportRecordsPath :: FilePath
  }
  deriving stock (Show)

data SupportedAccountAlias = Paypal | CommerzBank | Comdirect | Hand
  deriving stock (Show, Read, Eq, Enum, Bounded)

_SupportedAccountAlias :: Prism' AccAlias SupportedAccountAlias
_SupportedAccountAlias = prism' (view isoStrBs . show) (readMaybe . review isoStrBs)
