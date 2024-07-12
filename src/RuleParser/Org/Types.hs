module RuleParser.Org.Types where

import Data.Org
import Hledger (AccountName)
import MyPrelude
import Types

newtype ID = ID Text
  deriving stock (Show)
  deriving newtype (IsString, Eq, Ord)

data Ref = Ref URL Text deriving stock (Show, Eq)

data OrgAccHeading = OrgAccHeading
  { orgAccName :: AccountName
  , orgAccID :: Maybe ID
  , orgAccMatchers :: [LabelMatcher]
  }
  deriving stock (Show)

newtype OrgHeadingText = OrgHeadingText Text
  deriving stock (Show)
  deriving newtype (IsString, Eq, Ord)

data OrgAccountLabel = OrgAccountLabel
  { accLabel :: TaLabel
  , accLabelID :: ID
  , accabelCmtTag :: Maybe Text
  }
  deriving stock (Show)

data OrgSupportedAccount = OrgSupportedAccount
  { orgAccountRef :: Ref
  , orgAccountDateFormat :: DateFormat
  , orgAccountUnknownCommentLabels :: [TaLabel]
  , orgAccountRawRecordsName :: Text
  , orgAccountImportRecordsName :: Text
  , orgAccountLabels :: [OrgAccountLabel]
  }
  deriving stock (Show)

data LabelMatcher = LabelMatcher
  { labelRef :: Ref
  , labelMatchRule :: MatchRule
  , labelScore :: Maybe Score
  , labelMatcherStr :: [Text]
  }
  deriving stock (Show)

data Description = Description {dText :: Text, dAccRefs :: [Ref]}
  deriving stock (Show)

data OrgConfig = OrgConfig
  { cSupportedAccounts :: [OrgSupportedAccount]
  , cFilters :: [LabelMatcher]
  , cDescs :: [Description]
  , cAccounts :: [OrgAccHeading]
  , cPassingScore :: Score
  , cFinanceDir :: FilePath
  }
  deriving stock (Show)
