{-# LANGUAGE FlexibleContexts, OverloadedStrings, ConstraintKinds #-}

module Network.Salesforce where

import Data.Monoid
import Control.Applicative
import Control.Monad.Reader

import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as BSU

import Control.Failure
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Resource (MonadResource, ResourceT, MonadUnsafeIO, MonadThrow)
import Data.Conduit (runResourceT)
import Network.HTTP.Conduit

import Data.Aeson

import Network.Salesforce.Util
import Network.Salesforce.Types

connectSF :: (MonadBaseControl IO m, MonadResource m, Failure HttpException m) =>
             BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> BSU.ByteString -> Manager -> m SFToken
connectSF clientID clientSecret username password securityToken mgr = do
  req <- parseUrl "https://login.salesforce.com/services/oauth2/token"
  let bodyreq = urlEncodedBody [("grant_type", "password"),
                                ("client_id", clientID), ("client_secret", clientSecret),
                                ("username", username),
                                ("password", mconcat [password, securityToken])] req
  jsonUnpack =<< httpJSON bodyreq mgr

data QueryCursor a = QueryCursor { records :: [a], nextRecordsUrl :: Maybe BS.ByteString } deriving Show

instance FromJSON a => FromJSON (QueryCursor a) where
    parseJSON (Object v) = QueryCursor <$> (v .: "records") <*> (v .:? "nextRecordsUrl")
    parseJSON _ = fail "Invalid query result."

-- Querying Salesforce

query :: (SFContext m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) => 
           BSU.ByteString -> m b
query q = do
  query <- sfGet (mappend "/services/data/v26.0/query?q=" q)
  js <- sfAction $ query{responseTimeout = Nothing}
  jsonUnpack js

queryMore :: (SFContext m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) =>
               BSU.ByteString -> m b
queryMore nexturl = do
  req <- sfGet nexturl
  js <- sfAction req
  jsonUnpack js

queryRest :: (SFContext m, MonadBaseControl IO m, MonadResource m, FromJSON b, Failure HttpException m) =>
             [QueryCursor b] -> QueryCursor b -> m [b]
queryRest acc q = case nextRecordsUrl q of
                    Just url -> do
                      nextq <- queryMore url
                      queryRest (nextq:acc) nextq
                    Nothing -> return $ concatMap records acc


queryAll :: (SFContext m, MonadBaseControl IO m, MonadResource m, Failure HttpException m, FromJSON b) =>
              BSU.ByteString -> m [b]
queryAll qs = do
  fq <- query qs
  queryRest [fq] fq

-- DML Methods (insert, update, upsert, delete).

-- Runner methods to unpack SF Monadic Forms

type SFConnection m a = ReaderT (Manager, SFToken) (ResourceT m) a

runSF :: MonadBaseControl IO m => Manager -> SFToken -> SFConnection m a -> m a
runSF mgr token runner = runResourceT $ runReaderT runner (mgr, token)

runAuthSF :: MonadBaseControl IO m => (Manager -> m SFToken) -> SFConnection m b -> Manager -> m b
runAuthSF auth action mgr = do
  token <- auth mgr
  runSF mgr token action

runSingleAuthSF ::
  (MonadIO m, MonadBaseControl IO m, MonadUnsafeIO m, MonadThrow m) =>
  (Manager -> ResourceT m SFToken) -> SFConnection (ResourceT m) a -> m a
runSingleAuthSF login action = withManager $ \mgr -> runAuthSF login action mgr
