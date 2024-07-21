module RuleParser.Org.Parser where

import Data.Org
import MyPrelude
import Numeric.Lens (decimal)
import RIO.Text qualified as T
import RuleParser.Org.Types
import RuleParser.Utils
import Types

mkCustomLenses ''OrgFile
mkCustomLenses ''OrgDoc
mkCustomLenses ''Section
mkCustomLenses ''ListItems
mkCustomLenses ''Item
makePrisms ''Words
makePrisms ''Block

mkCustomLenses ''OrgAccHeading
mkCustomLenses ''OrgAccountLabel
mkCustomLenses ''OrgSupportedAccount
mkCustomLenses ''LabelMatcher
mkCustomLenses ''OrgConfig

type Parser s a = Getter s (Either Text a)
parseRight :: Parser s a -> Getter s a
parseRight p = p . to unwrap
  where
    unwrap (Left msg) = error $ displayException $ ParseRuleException (T.unpack msg)
    unwrap (Right v) = v

make :: Getter s (Maybe a) -> (s -> Text) -> Parser s a
make parse errMsg = to $ \s -> maybeToEither (errMsg s) (s ^. parse)

sectionInfo :: Section -> Text
sectionInfo s =
  "Section " <> wrapShow "with heading [" (s ^. sectionHeadingL) "]"

propL :: Text -> Parser Section Text
propL key =
  make
    (sectionPropsL . at key)
    (\s -> "property " <> tshow key <> " not found in: " <> sectionInfo s)

idL :: Parser Section ID
idL = propL "ID" . coerced

dateFormatL :: Getter Section DateFormat
dateFormatL = propL "date-format" . ifLeft "%d.%m.%Y" . re isoBsT

matchRuleL :: Getter Section MatchRule
matchRuleL =
  to $ \x -> x ^. propL "match-rule" ^? _Right . to parseMatchRule ^. non Contains

accHeadingsL :: Getter Section [OrgAccHeading]
getAccHeadings :: Section -> [OrgAccHeading]
accHeadingsL = to getAccHeadings
getAccHeadings s =
  let
    OrgHeadingText txt = s ^. sectionHeadingL . orgHeadingTextL
    hid = s ^. idL ^? _Right
    (hds, lmatchers) =
      partitionEithers
        $ s
        ^. childrenSections simple
        ^.. traverse
        . tryGet (labelMatcherL . to eitherToMaybe)
   in
    pureIf
      (isJust hid || not (null lmatchers) || null hds)
      (OrgAccHeading txt hid lmatchers)
      <> ( hds
            ^.. each
            . accHeadingsL
            . folded
            & each
            . orgAccNameL
            %~ ((txt <> ":") <>)
         )

-- assume the given section is the root account section
allAccHeadings :: Section -> [OrgAccHeading]
allAccHeadings s = s ^. childrenSections accHeadingsL ^.. traverse . traverse

parseOrgConfig :: OrgFile -> Either Text OrgConfig
parseOrgConfig f = do
  (rules, accs) <- case f ^. orgDocL . docSectionsL of
    rs : as : _ -> Right (rs, allAccHeadings as)
    unknown -> Left $ "Org config should contain at least one heading for parse rules and one heading for Accounts, but got " <> tshow (length unknown) <> " sections"
  (supportedAccs, filters, descs) <- case rules ^. sectionDocL . docSectionsL of
    [supportedAccs, filters, descs] ->
      Right
        ( supportedAccs ^. childrenSections (parseRight supportedAccountL)
        , filters ^. childrenSections (parseRight labelMatcherL)
        , descs ^. childrenSections descriptionL
        )
    _ -> Left "Parse rules should have exactly three headings for Supported Accounts,filters and Descriptions"
  ps <- rules ^. propL "passing-score" <&> parseScore
  fd <- rules ^. propL "finance-dir" <&> view (re isoStrT)
  return $ OrgConfig supportedAccs filters descs accs ps fd

refL :: Parser Words Ref
refL = to parse
  where
    parse (Link url (Just t)) = Right $ Ref url t
    parse ws = Left $ "Expect reference, got: " <> tshow ws

firstWords :: Parser Words a -> Parser (NonEmpty Words) a
firstWords g = to $ \ws -> ws ^.. each . g ^.. folded . _Right ^? _head ^. err
  where
    err = to (maybeToEither "No words can be parsered correctly")

orgAccountLabelL :: Parser Section OrgAccountLabel
orgAccountLabelL = to $ \s -> do
  lid <- s ^. idL
  let OrgHeadingText label = s ^. sectionHeadingL . orgHeadingTextL
      cmtTag = s ^. propL "comment-tag" ^? _Right
  return $ OrgAccountLabel label lid cmtTag

asList :: Text -> [Text]
asList = fmap T.strip . T.split (',' ==)

supportedAccountL :: Parser Section OrgSupportedAccount
supportedAccountL = to $ \s -> do
  let df = s ^. dateFormatL
  ref <- s ^. sectionHeadingL . firstWords refL
  unknownLbls <- s ^. propL "unknown-comment-labels" <&> asList
  rawRecordsName <- s ^. propL "raw-records-name"
  importRecordsName <- s ^. propL "import-records-name"
  let labels = s ^. childrenSections (orgAccountLabelL . _Right)
  return
    $ OrgSupportedAccount ref df unknownLbls rawRecordsName importRecordsName labels

childrenSections :: Fold Section a -> Getter Section [a]
childrenSections g = to $ \s -> s ^. sectionDocL . docSectionsL ^.. each . g

descriptionL :: Getter Section Description
descriptionL = to $ \s ->
  let
    refs = s ^. sectionListWordsL (firstWords refL) ^.. folded . _Right
    OrgHeadingText txt = s ^. sectionHeadingL . orgHeadingTextL
   in
    Description txt refs

labelMatcherL :: Parser Section LabelMatcher
labelMatcherL = to $ \s -> do
  let mr = s ^. matchRuleL
  let score = s ^. propL "score" ^? _Right . to parseScore
  ref <- s ^. sectionHeadingL . firstWords refL
  let str = s ^. sectionListWordsL plainTextL
  return $ LabelMatcher ref mr score str

plainTextL :: Getter (NonEmpty Words) Text
plainTextL = to $ \orgWords -> T.unwords $ orgWords ^.. folded . _Plain

orgHeadingTextL :: Getter (NonEmpty Words) OrgHeadingText
orgHeadingTextL = plainTextL . coerced

listItemsL :: Getter ListItems (NonEmpty Item)
listItemsL = to $ \(ListItems _ r) -> r

itemWordsL :: Getter Item (NonEmpty Words)
itemWordsL = to $ \(Item ws _) -> ws

sectionListWordsL :: Getter (NonEmpty Words) a -> Getter Section [a]
sectionListWordsL g = to $ \s ->
  s
    ^. (sectionDocL . docBlocksL)
    ^.. (folded . _List . listItemsL . traverse . itemWordsL . g)
