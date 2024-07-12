module RuleParser.Org.Conv where

import Data.Org
import Hledger (AccountName)
import MyPrelude
import RIO.ByteString.Lazy qualified as BL
import RIO.Map qualified as Map
import RIO.State
import RIO.Text qualified as T
import RuleParser.Org.Parser
import RuleParser.Org.Types
import RuleParser.Utils
import System.IO (print)
import Types

type AccID = ID
type LabelID = ID

refToId :: (MonadThrow m) => Ref -> m ID
refToId (Ref (URL (T.stripPrefix "id:" -> Just suf)) _) = return (ID suf)
refToId r =
  throwM $ ParseRuleException $ "reference must start with 'id:': " <> show r

data OrgEnv = OrgEnv
  { _orgAccMap :: Map AccID AccountName
  , _orgLabelMap :: Map LabelID (AccountName, TaLabel)
  , _orgAccountRules :: Map AccountName AccountRule
  , _orgPassingScore :: Score
  }
  deriving stock (Show, Generic)
  deriving anyclass (Default)
makeClassy ''OrgEnv

type OrgM env m = (MonadState env m, HasOrgEnv env, MonadThrow m)

initState :: (OrgM env m) => OrgConfig -> m ()
initState (OrgConfig {..}) = do
  orgPassingScore .= cPassingScore
  orgAccMap .= Map.fromList [(hid, acc) | OrgAccHeading acc (Just hid) _ <- cAccounts]
  accLbls <- forM cSupportedAccounts $ \(OrgSupportedAccount {..}) -> do
    acc <- findAcc orgAccountRef
    let
      orgCmtTags =
        [ (encodeUtf8 tag, lbl)
        | OrgAccountLabel lbl _ (Just tag) <- orgAccountLabels
        ]
      Ref _ alias = orgAccountRef
      ar =
        AccountRule
          acc
          (encodeUtf8 alias)
          orgAccountDateFormat
          orgCmtTags
          orgAccountUnknownCommentLabels
          []
          []
          (orgAccountRawRecordsName ^. re isoStrT)
          (orgAccountImportRecordsName ^. re isoStrT)
    orgAccountRules . at acc ?= ar
    return $ [(lid, (acc, lbl)) | OrgAccountLabel lbl lid _ <- orgAccountLabels]
  orgLabelMap .= Map.fromList (concat accLbls)

findAcc :: (OrgM env m) => Ref -> m AccountName
findAcc ref = do
  accs <- gets (view orgAccMap)
  hid <- refToId ref
  forceJust
    (Map.lookup hid accs)
    (ParseRuleException $ "could not find account by " <> show ref)

convLabelMatcher :: (OrgM env m) => a -> LabelMatcher -> m (FromAcc, LabelCG a)
convLabelMatcher a (LabelMatcher {..}) = do
  lbls <- gets (view orgLabelMap)
  dftScore <- gets (view orgPassingScore)
  lblID <- refToId labelRef
  (acc, lbl) <-
    forceJust
      (Map.lookup lblID lbls)
      (ParseRuleException $ "could not find label by " <> show labelRef)
  let cg =
        labelMatcherStr <&> \str ->
          ( Matcher labelMatchRule (T.encodeUtf8 str)
          , (a, fromMaybe dftScore labelScore)
          )
  return (acc, (lbl, cg))

toImportRule :: (MonadThrow m, LoggerM env m) => OrgConfig -> m ImportRule
toImportRule oc@(OrgConfig {..}) = flip evalStateT (def :: OrgEnv) do
  initState oc
  descs <- forM cDescs $ \(Description t refs) -> do
    accs <- traverse findAcc refs
    return $ accs <&> (,encodeUtf8 t)
  filters <- traverse (convLabelMatcher ()) cFilters
  forM_ filters
    $ \(acc, f) -> orgAccountRules . at acc %= fmap (over accFilter (f :))
  accCatchers <- forM cAccounts
    $ \(OrgAccHeading acc _ matchers) -> traverse (convLabelMatcher acc) matchers
  forM_ (mergeList $ concat accCatchers)
    $ \(acc, cs) ->
      orgAccountRules
        . at acc
        %= fmap (set accCatcher (groupLabelCGs cs))
  ars <- gets (view orgAccountRules)
  logDebug $ displayShow $ ars ^. at "asserts:paypal" . _Just . accCatcher . to show
  return
    $ ImportRule
      (Map.elems ars)
      (Map.fromList $ concat descs)
      cPassingScore
      cFinanceDir

readOrgConfig :: (MonadIO m, MonadThrow m) => FilePath -> m OrgConfig
readOrgConfig p = do
  f <- readFileUtf8 p
  orgF <- forceJust (org f) $ ParseRuleException $ "could not parse " <> p <> " as OrgFile"
  forceRight
    $ mapLeft
      (ParseRuleException . ("could not parse OrgConfig: " <>) . T.unpack)
      (parseOrgConfig orgF)
