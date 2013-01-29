{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}

module Network.Salesforce where

import Data.Monoid
import Control.Applicative
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

import Control.Monad.Trans.Control(MonadBaseControl)
import Data.Conduit (ResourceT, runResourceT)
import Network.HTTP.Conduit

import Data.Aeson

import Network.Salesforce.Util
import Network.Salesforce.Types

connectSF :: (RestIO m) => BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Manager -> m SFToken
connectSF clientID clientSecret username password securityToken mgr = do
  req <- parseUrl "https://login.salesforce.com/services/oauth2/token"
  let bodyreq = urlEncodedBody [("grant_type", "password"),
                                ("client_id", clientID), ("client_secret", clientSecret),
                                ("username", username),
                                ("password", mconcat [password, securityToken])] req
  jsonUnpack =<< httpJSON bodyreq mgr

-- Querying Salesforce

data QueryCursor a = QueryCursor { records :: [a], nextRecordsUrl :: Maybe BS.ByteString } deriving Show

instance FromJSON a => FromJSON (QueryCursor a) where
    parseJSON (Object v) = QueryCursor <$> (v .: "records") <*> (v .:? "nextRecordsUrl")
    parseJSON _ = fail "Invalid query result."

query :: (SFContext m, RestIO m, FromJSON b) => BSU.ByteString -> m b
query q = sfGet (mappend "/services/data/v26.0/query?q=" q) >>= sfJSON

queryMore :: (SFContext m, RestIO m, FromJSON b) => BSU.ByteString -> m b
queryMore nexturl = sfGet nexturl >>= sfJSON

queryRest :: (SFContext m, RestIO m, FromJSON b) => QueryCursor b -> m [b]
queryRest q = 
    queryRest' [] q
    where
      queryRest' acc q' = case nextRecordsUrl q' of
                            Just url -> do
                              nextq <- queryMore url
                              queryRest' (nextq:acc) nextq
                            Nothing -> return $ concatMap records acc

queryAll :: (SFContext m, RestIO m, FromJSON b) => BSU.ByteString -> m [b]
queryAll qs = do
  fq <- query qs
  queryRest fq

-- DML Methods (insert, update, upsert, delete).
insert :: (RestIO m, SFContext m, ToJSON a, FromJSON b) => BSU.ByteString -> a -> m b
insert klass js = do 
  insertion <- sfPost $ mconcat ["/sobjects/", klass, "/"]
  sfJSON insertion{requestBody = RequestBodyLBS $ encode js}

runSF :: MonadBaseControl IO m => Manager -> SFToken -> SFConnection m a -> m a
runSF mgr token runner = runResourceT $ runReaderT runner (mgr, token)

runAuthSF :: MonadBaseControl IO m => (Manager -> m SFToken) -> SFConnection m b -> Manager -> m b
runAuthSF auth action mgr = do
  token <- auth mgr
  runSF mgr token action

runSingleAuthSF :: SFRunnable m => (Manager -> ResourceT m SFToken) -> SFConnection (ResourceT m) a -> m a
runSingleAuthSF login action = withManager $ \mgr -> runAuthSF login action mgr
