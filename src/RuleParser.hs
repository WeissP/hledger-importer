module RuleParser where

import MyPrelude

import Control.Monad.Extra (pureIf)
import Data.Either.Extra (eitherToMaybe, maybeToEither)
import Data.List.Extra (notNull)
import Data.Org
import Hledger (AccountName)
import MyPrelude
import Numeric.Lens (decimal)
import RIO.Text qualified as T

-- import Types.Org
import Types.Record
