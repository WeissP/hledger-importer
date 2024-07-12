module Catch where

import Data.ByteString qualified as B
import Hledger (accountLeafName)
import MyPrelude
import RIO.List qualified as L
import RIO.Map qualified as Map
import Text.Regex.TDFA ((=~))
import Types

match :: ByteString -> Matcher -> Bool
match doc (Matcher Contains str) = str `B.isInfixOf` doc
match doc (Matcher Regexp str) = doc =~ str

catchByGroup ::
  (Ord item) =>
  CatchGroup item ->
  ByteString ->
  Map item Score
catchByGroup cg doc =
  Map.fromListWith
    (+)
    [award | (matcher, award) <- cg, doc `match` matcher]

catchItem ::
  (Show item, Ord item, ImporterM env m) =>
  (TaLabel -> MaybeT m ByteString) ->
  [LabelCG item] ->
  m (Maybe item)
catchItem byLabel lcgs = runMaybeT do
  let go (lbl, cg) = fromMaybeM (return def) $ runMaybeT do
        doc <- byLabel lbl
        return $ catchByGroup cg doc
  logDebug $ "lcgs: " <> displayShow lcgs
  merged <- lift $ traverse go lcgs <&> foldr (Map.unionWith (+)) def
  logDebug $ "catch score map: " <> displayShow merged
  minScore <- view irPassingScore
  (item, score) <-
    hoistMaybe $ L.maximumByMaybe (compare `on` snd) $ Map.toList merged
  pureIf (score >= minScore) item

catchDesc :: (ImporterM env m) => ToAcc -> m ByteString
catchDesc acc = do
  dm <- view irDescMap
  return $ fromMaybe (encodeUtf8 $ accountLeafName acc) (Map.lookup acc dm)
