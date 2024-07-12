module Types.ImportRule where

import Hledger (AccountName)
import MyPrelude
import RIO.Map qualified as Map

type Score = Int
type FromAcc = AccountName
type ToAcc = AccountName
type AccAlias = ByteString
type TaLabel = Text
type Award a = (a, Score)
type CatchGroup a = [(Matcher, Award a)]
type LabelCG a = (TaLabel, CatchGroup a)
type DateFormat = ByteString

data Matcher = Matcher {matchRule :: MatchRule, matchStr :: ByteString}
  deriving stock (Show)

data MatchRule = Contains | Regexp deriving stock (Show, Eq)
makePrisms ''MatchRule

groupLabelCGs :: [LabelCG a] -> [LabelCG a]
groupLabelCGs = Map.toList . Map.fromListWith (<>)

data AccountRule = AccountRule
  { _accName :: FromAcc
  , _accAlias :: AccAlias
  , _accDateFormat :: DateFormat
  , _accCommentTags :: [(ByteString, TaLabel)]
  , _accUnknownCommentLabels :: [TaLabel]
  , _accFilter :: [LabelCG ()]
  , _accCatcher :: [LabelCG ToAcc]
  , _accRawRecordsName :: String
  , _accImportRecordsName :: String
  }
  deriving stock (Show)
makeClassy ''AccountRule

data ImportRule = ImportRule
  { _irAccRules :: [AccountRule]
  , _irDescMap :: Map ToAcc ByteString
  , _irPassingScore :: Score
  , _irFinanceDir :: FilePath
  }
  deriving stock (Show)
makeClassy ''ImportRule

type ImporterM env m = (MonadReader env m, HasLogFunc env, HasImportRule env, MonadThrow m, MonadIO m)
instance
  (HasImportRule env) =>
  (HasImportRule (WithLogFunc env))
  where
  importRule = wEnv . importRule

type InvalidRecord = Utf8Builder

data ImportException = ImportException AccAlias String
instance Show ImportException where
  show (ImportException alias msg) =
    "fatal error during importing account " <> show alias <> ": " <> msg
instance Exception ImportException
