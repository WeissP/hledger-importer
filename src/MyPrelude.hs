{-# OPTIONS_GHC -Wno-orphans #-}

module MyPrelude (
  module X,
  module Data.Default,
  module RIO,
  module Control.Monad.Except,
  module Control.Monad.Extra,
  getEnvDft,
  getEnv,
  maybeToEither,
  wrapShow,
  logException,
  firstJustsM,
  buildUTF8,
  displayWithSep,
  wrapDisplay,
  mergeList,
  listDisplay,
  getOrDft,
  replace,
  forceRight,
  forceJust,
  LoggerM,
  dropBOM,
  isoBsT,
  isoStrT,
  WithLogFunc (..),
  wlf,
  isoStrBs,
  wEnv,
  Parse,
  getIn,
  getStrIn,
  withItems,
  logIfNothing,
  module Data.Either.Extra,
) where

import Control.Lens as X
import Control.Monad.Except
import Control.Monad.Extra hiding (mapMaybeM, unlessM, whenM)
import Control.Monad.Trans.Maybe as X
import Data.Aeson qualified as J
import Data.Aeson.Lens (key, _Array, _String)
import Data.ByteString.Builder
import Data.ByteString.Char8 qualified as BSC
import Data.Default
import Data.Either.Extra (eitherToMaybe)
import Data.Foldable (foldlM)
import Data.Semigroup (Semigroup (..))
import RIO hiding (ASetter, ASetter', Getting, Lens, Lens', lens, over, preview, set, sets, to, view, (%~), (.~), (^.), (^..), (^?))
import RIO.ByteString qualified as B
import RIO.ByteString.Lazy qualified as BL
import RIO.List qualified as L
import RIO.Map qualified as Map
import RIO.NonEmpty qualified as NE
import RIO.Text qualified as T
import System.Environment qualified as S
import System.Environment.Blank qualified as SB

class ItemsDisplay f where
  displayWithSep :: (Display a) => Builder -> f a -> Utf8Builder
  listDisplay :: (Display a) => f a -> Utf8Builder
  listDisplay items = wrapDisplay "[" (displayWithSep ", " items) "]"

instance ItemsDisplay NonEmpty where
  displayWithSep sep items =
    Utf8Builder
      $ sconcat
      $ NE.intersperse sep (items <&> getUtf8Builder . display)

instance ItemsDisplay [] where
  displayWithSep sep items = case NE.nonEmpty items of
    Just it -> displayWithSep sep it
    Nothing -> ""

instance Display String where display = fromString
instance Display ByteString where display = Utf8Builder . byteString

buildUTF8 :: (Display a) => a -> ByteString
buildUTF8 = BL.toStrict . toLazyByteString . getUtf8Builder . display

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just b) = Right b
maybeToEither a Nothing = Left a

wrapShow :: (Show a) => Text -> a -> Text -> Text
wrapShow l a r = l <> tshow a <> r

wrapDisplay :: (Display a) => Builder -> a -> Builder -> Utf8Builder
wrapDisplay l a r = Utf8Builder $ l <> getUtf8Builder (display a) <> r

logException ::
  ( MonadIO m
  , MonadReader env m
  , HasLogFunc env
  , HasCallStack
  , Exception e
  ) =>
  e ->
  m ()
logException = logWarn . ("Catched Exception: " <>) . fromString . displayException

forceJust :: (MonadThrow m, Exception e) => Maybe a -> e -> m a
forceJust (Just a) _ = return a
forceJust Nothing s = throwM s

forceRight :: (MonadThrow m, Exception e) => Either e a -> m a
forceRight (Right a) = return a
forceRight (Left e) = throwM e

{- | Takes computations returnings @Maybes@; tries each one in order.
The first one to return a @Just@ wins. Returns @Nothing@ if all computations
return @Nothing@.
(comes from GHC-9.8.2)
-}
firstJustsM :: (Monad m, Foldable f) => f (m (Maybe a)) -> m (Maybe a)
firstJustsM = foldlM go Nothing
  where
    go :: (Monad m) => Maybe a -> m (Maybe a) -> m (Maybe a)
    go Nothing action = action
    go result@(Just _) _action = return result

getOrDft :: (Default a) => Maybe a -> a
getOrDft (Just v) = v
getOrDft Nothing = def

replace :: Lens' a b -> a -> a -> a
replace l base new = base & l .~ new ^. l

getEnvDft :: (MonadIO m) => String -> String -> m String
getEnvDft name dft = liftIO $ SB.getEnvDefault name dft

getEnv :: (MonadIO m) => String -> m String
getEnv = liftIO . S.getEnv

type LoggerM env m =
  ( MonadUnliftIO m
  , MonadReader env m
  , HasLogFunc env
  )

instance Show Utf8Builder where
  show = T.unpack . textDisplay

dropBOM :: ByteString -> ByteString
dropBOM bs
  | B.take 3 bs == B.pack [239, 187, 191] = B.drop 3 bs
  | otherwise = bs

isoBsT :: Iso' ByteString Text
isoBsT = iso decodeUtf8Lenient encodeUtf8

isoStrT :: Iso' String Text
isoStrT = iso T.pack T.unpack

isoStrBs :: Iso' String ByteString
isoStrBs = iso BSC.pack BSC.unpack

mergeList :: (Ord k) => [(k, v)] -> [(k, [v])]
mergeList l = Map.toList $ Map.fromListWith (<>) (second (: []) <$> l)

data WithLogFunc a = WithLogFunc
  { _wlf :: LogFunc
  , _wEnv :: a
  }
makeLenses ''WithLogFunc
instance HasLogFunc (WithLogFunc a) where
  logFuncL = wlf

type Parse s a = forall pm. (Monoid pm) => Getting pm s a
getIn :: [J.Key] -> Parse J.Value J.Value
getIn = foldr ((.) . key) simple

getStrIn :: [J.Key] -> Parse J.Value ByteString
getStrIn keys = getIn keys . _String . re isoBsT

withItems :: Parse J.Value J.Value -> Parse J.Value ByteString
withItems p = to $ \t ->
  t
    ^. (getIn ["cart_info", "item_details"] . _Array)
    ^.. (traverse . p . _String . re isoBsT)
    & B.intercalate " | "

logIfNothing :: (LoggerM env m) => MaybeT m a -> m () -> MaybeT m a
logIfNothing task l = MaybeT do
  r <- runMaybeT task
  case r of
    Nothing -> l >> return Nothing
    j -> return j
