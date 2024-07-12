module RuleParser.Utils where

import Control.Lens.Operators
import Control.Lens.TH
import Language.Haskell.TH
import MyPrelude
import Numeric.Lens (decimal)
import Types

mkCustomLenses :: Name -> DecsQ
mkCustomLenses =
  makeLensesWith
    $ lensRules
    & lensField
      .~ (\_ _ name -> [TopName (mkName $ nameBase name <> "L")])

ifLeft :: b -> Iso' (Either a b) b
ifLeft b = iso (fromRight b) Right

parseMatchRule :: Text -> MatchRule
parseMatchRule "Contains" = Contains
parseMatchRule "contains" = Contains
parseMatchRule "Regexp" = Regexp
parseMatchRule "regexp" = Regexp
parseMatchRule unknown =
  error
    $ "could not parse "
    <> unknown
    ^. re isoStrT
      <> " as MatchRule"

parseScore :: Text -> Score
parseScore str = case str ^. from isoStrT ^? decimal of
  Just s -> s
  Nothing -> error $ "failed to parse " <> str ^. re isoStrT <> " as score"

tryGet :: Getter s (Maybe a) -> Getter s (Either s a)
tryGet g = to $ \s -> maybeToEither s (s ^. g)

newtype ParseRuleException = ParseRuleException String
instance Show ParseRuleException where
  show (ParseRuleException msg) =
    "error during parsing rule " <> ": " <> msg
instance Exception ParseRuleException
